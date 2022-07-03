use auxtools::{
    raw_types::values::ValueTag, runtime, DMResult, List, Proc, Runtime, StringRef, WeakValue,
};
use mlua::{FromLua, Lua, MultiValue, ToLua, UserData, UserDataFields, UserDataMethods};
use std::cell::RefCell;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::iter::FromIterator;
use std::time::Instant;

pub type DMValue = auxtools::Value;
pub type MluaValue<'lua> = mlua::Value<'lua>;

#[macro_export]
macro_rules! specific_runtime {
	($fmt:expr) => {
		runtime!("Rust error at {},{}: {}", std::file!(), std::line!(), $fmt)
	};
	($fmt: expr, $( $args:expr ),*) => {
		runtime!("Rust error at {},{}: {}", std::file!(), std::line!(), format!( $fmt, $( $args, )* ))
	};
}

#[macro_export]
macro_rules! external {
    ($lit:literal) => {
        mlua::Error::external(AuxluaError::new_from_str($lit))
    };
    ($fmt:expr) => {
        mlua::Error::external(AuxluaError($fmt))
    };
    ($fmt:expr, $( $args:expr ),*) => {
        mlua::Error::external(AuxluaError(format!($fmt, $($args,)*)))
    }
}

#[macro_export]
macro_rules! specific_external {
	($fmt:expr) => {
		external!("Rust error at {},{}: {}", std::file!(), std::line!(), $fmt)
	};
	($fmt: expr, $( $args:expr ),*) => {
		external!("Rust error at {},{}: {}", std::file!(), std::line!(), format!( $fmt, $( $args, )* ))
	};
}

thread_local! {
    pub static LUA_THREAD_START: RefCell<Instant> = RefCell::new(Instant::now());
    pub static SET_VAR_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
    pub static DATUM_CALL_PROC_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
    pub static GLOBAL_CALL_PROC_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
    pub static PRINT_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
}

#[derive(Debug)]
pub struct AuxluaError(pub String);

impl Display for AuxluaError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for AuxluaError {}

impl AuxluaError {
    pub fn new_from_str(s: &str) -> Self {
        Self(String::from(s))
    }
}

#[derive(Clone)]
pub struct ListWrapper {
    pub value: DMValue,
}

impl From<ListWrapper> for DMValue {
    fn from(wrapper: ListWrapper) -> Self {
        wrapper.value
    }
}

impl<T> PartialEq<T> for ListWrapper
where
    T: Clone + Into<DMValue>,
{
    fn eq(&self, other: &T) -> bool {
        self.value == other.clone().into()
    }
}

impl UserData for ListWrapper {
    fn add_fields<'lua, F: UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("len", |_, this| {
            this.value
                .as_list()
                .map_err(|_| external!("not a list"))
                .map(|list| list.len())
        });
    }

    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(mlua::MetaMethod::Eq, datum_equality);

        methods.add_method("get", |_, this, key: Value| {
            this.value
                .as_list()
                .map_err(|_| runtime!("not a list"))
                .and_then(|list| {
                    DMValue::try_from(key).and_then(|key_val| {
                        list.get(key_val).and_then(|value| Value::try_from(&value))
                    })
                })
                .map_err(|e| external!(e.message))
        });

        methods.add_method("set", |_, this, (key, value): (Value, Value)| {
            this.value
                .as_list()
                .map_err(|_| runtime!("not a list"))
                .and_then(|list| {
                    DMValue::try_from(key)
                        .and_then(|key_val| list.set(key_val, DMValue::try_from(value)?))
                })
                .map_err(|e| external!(e.message))
        });

        methods.add_method("add", |_, this, elem: Value| {
            this.value
                .as_list()
                .map_err(|_| runtime!("not a list"))
                .and_then(|list| {
                    DMValue::try_from(elem).map(|elem_val| {
                        list.append(elem_val);
                        mlua::Nil
                    })
                })
                .map_err(|e| external!(e.message))
        });

        methods.add_method("to_table", |_, this, _: MultiValue| {
            this.value
                .as_list()
                .map_err(|_| runtime!("not a list"))
                .and_then(tablify_list)
                .map_err(|e| external!(e.message))
        });
    }
}

/// Converts a DM list into a lua table.
pub fn tablify_list(list: auxtools::List) -> DMResult<Value> {
    let mut vec: Vec<(Value, Value)> = Vec::new();
    for i in 1..=list.len() {
        let key = &list.get(i)?;
        if key.raw.tag == ValueTag::Null {
            continue;
        }
        let list_value = &list.get(key).unwrap_or_else(|_| DMValue::null());
        if list_value.raw.tag != ValueTag::Null {
            vec.push((Value::try_from(key)?, Value::try_from(list_value)?));
        } else {
            vec.push((Value::Number(i as f32), Value::try_from(key)?))
        }
    }
    Ok(Value::List(vec))
}

#[derive(Copy, Clone)]
pub struct DatumWrapper {
    pub value: WeakValue,
}

impl From<DatumWrapper> for DMValue {
    fn from(wrapper: DatumWrapper) -> Self {
        wrapper.value.upgrade_or_null()
    }
}

impl<T> PartialEq<T> for DatumWrapper
where
    T: Clone + Into<DMValue>,
{
    fn eq(&self, other: &T) -> bool {
        self.value.upgrade_or_null() == other.clone().into()
    }
}

fn datum_call_proc<'lua>(
    lua: &'lua Lua,
    datum: &DMValue,
    args: MultiValue<'lua>,
) -> mlua::Result<Value> {
    let mut args_iter = args.into_iter();
    let first_arg: MluaValue<'lua> = args_iter.next().ok_or_else(|| {
        external!("attempted call with 0 arguments (expected proc name and 0+ proc arguments)")
    })?;
    let proc = String::from_lua(first_arg, lua)?;

    // Try to convert the args into intermediary value structs
    let proc_args =
        args_iter.try_fold::<_, _, mlua::Result<_>>(Vec::<Value>::new(), |mut acc, val| {
            let arg = Value::from_lua(val, lua)?;
            acc.push(arg);
            Ok(acc)
        })?;

    // Try to convert the intermediary values into DM values
    let converted: Vec<DMValue> = proc_args
        .into_iter()
        .map(DMValue::try_from)
        .collect::<Result<Vec<DMValue>, Runtime>>()
        .map_err(|e| external!(e.message))?;

    let call_result = DATUM_CALL_PROC_WRAPPER.with(|wrapper| match &*wrapper.borrow() {
        // If there is a wrapper proc, call it
        Some(wrapper_name) => {
            let wrapper_proc = Proc::find(wrapper_name)
                .ok_or_else(|| specific_runtime!("{} not found", wrapper_name))?;
            let wrapped_args = &DMValue::from(List::from_iter(converted));
            wrapper_proc.call(&[datum, &DMValue::from_string(proc).unwrap(), wrapped_args])
        }
        // If there is no wrapper proc, directly call the proc with the args
        None => datum.call(proc, converted.iter().collect::<Vec<&DMValue>>().as_slice()),
    });
    match call_result {
        Ok(ret) => Value::try_from(&ret).map_err(|e| external!(e.message)),
        Err(e) => Err(external!(e.message)),
    }
}

fn datum_set_var(datum: &DMValue, var: String, value: Value) -> DMResult<()> {
    DMValue::from_string(var).and_then(|var_string| {
        // Convert the value to DM
        let value = &DMValue::try_from(value)?;
        SET_VAR_WRAPPER.with(|wrapper| match &*wrapper.borrow() {
            // If there is a wrapper proc, call it
            Some(wrapper_name) => {
                let wrapper_proc = Proc::find(wrapper_name)
                    .ok_or_else(|| specific_runtime!("{} not found", wrapper_name))?;
                match wrapper_proc.call(&[datum, &var_string, value]) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(specific_runtime!(e.message)),
                }
            }
            // If not, directly set the var
            None => datum.set(StringRef::from_value(var_string).unwrap(), value),
        })
    })
}

fn datum_to_string(_: &Lua, arg: Value) -> mlua::Result<String> {
    DMValue::try_from(arg)
        .and_then(|value| value.to_string())
        .map_err(|e| external!(e.message))
}

impl UserData for DatumWrapper {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(mlua::MetaMethod::Eq, datum_equality);
        methods.add_meta_function(mlua::MetaMethod::ToString, datum_to_string);

        methods.add_method("get_var", |_, this, var: String| {
            this.value
                .upgrade()
                .ok_or_else(|| runtime!("datum was deleted"))
                .and_then(|datum| {
                    StringRef::from_raw(var.as_bytes()).and_then(|var_ref| {
                        datum.get(var_ref).and_then(|value| Value::try_from(&value))
                    })
                })
                .map_err(|e| external!(e.message))
        });

        methods.add_method("set_var", |_, this, args: (String, Value)| {
            let (var, value) = args;
            this.value
                .upgrade()
                .ok_or_else(|| runtime!("datum was deleted"))
                .and_then(|datum| datum_set_var(&datum, var, value))
                .map_err(|e| external!(e.message))
        });

        methods.add_method("call_proc", |lua, this, args: MultiValue| {
            let datum = this
                .value
                .upgrade()
                .ok_or_else(|| external!("datum was deleted"))?;
            datum_call_proc(lua, &datum, args)
        });
    }
}

impl DatumWrapper {
    pub fn new(value: &DMValue) -> mlua::Result<Self> {
        value
            .as_weak()
            .map_err(|_| mlua::Error::FromLuaConversionError {
                from: "datum",
                to: "userdata",
                message: Some(String::from("weak value creation failed")),
            })
            .map(|weak_val| Self { value: weak_val })
    }
}

#[derive(Clone)]
pub struct GlobalWrapper {
    pub value: DMValue,
}

impl From<GlobalWrapper> for DMValue {
    fn from(wrapper: GlobalWrapper) -> Self {
        wrapper.value
    }
}

impl<T> PartialEq<T> for GlobalWrapper
where
    T: Clone + Into<DMValue>,
{
    fn eq(&self, other: &T) -> bool {
        self.value == other.clone().into()
    }
}

fn datum_equality(_: &Lua, args: (Value, Value)) -> mlua::Result<bool> {
    Ok(args.0 == args.1)
}

impl UserData for GlobalWrapper {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(mlua::MetaMethod::Eq, datum_equality);
        methods.add_meta_function(mlua::MetaMethod::ToString, datum_to_string);

        methods.add_method("get_var", |_, this, var: String| {
            StringRef::from_raw(var.as_bytes())
                .and_then(|var_ref| {
                    this.value
                        .get(var_ref)
                        .and_then(|value| Value::try_from(&value))
                })
                .map_err(|e| external!(e.message))
        });

        methods.add_method("set_var", |_, this, args: (String, Value)| {
            let (var, value) = args;
            datum_set_var(&this.value, var, value).map_err(|e| external!(e.message))
        });

        methods.add_method("call_proc", |lua, this, args: MultiValue| {
            datum_call_proc(lua, &this.value, args)
        });
    }
}

impl GlobalWrapper {
    pub fn new(value: DMValue) -> Self {
        Self { value }
    }
}

pub fn global_proc_call<'a>(lua: &'a mlua::Lua, args: MultiValue<'a>) -> mlua::Result<Value> {
    let mut args_iter = args.into_iter();

    // Convert the first arg into a string.
    let first_arg: MluaValue<'a> = args_iter.next().ok_or_else(|| {
        external!("attempted call with 0 arguments (expected proc name and 0+ proc arguments)")
    })?;
    let mut proc_name = String::from_lua(first_arg, lua)?;
    if proc_name.starts_with("__lua") {
        return Err(external!("attempted to call lua hook from within lua code"));
    };

    // Convert the rest of the args into intermediary value structs
    let proc_args =
        args_iter.try_fold::<_, _, mlua::Result<_>>(Vec::<Value>::new(), |mut acc, val| {
            let arg = Value::from_lua(val, lua)?;
            acc.push(arg);
            Ok(acc)
        })?;

    // Convert the intermediary values into DM values
    let converted: Vec<DMValue> = proc_args
        .into_iter()
        .map(DMValue::try_from)
        .collect::<Result<Vec<DMValue>, Runtime>>()
        .map_err(|e| external!(e.message))?;

    let call_result = GLOBAL_CALL_PROC_WRAPPER.with(|wrapper| match &*wrapper.borrow() {
        // If there is a wrapper proc, call it
        Some(wrapper_name) => {
            let wrapper_proc = Proc::find(wrapper_name)
                .ok_or_else(|| specific_runtime!("{} not found", wrapper_name))?;
            let wrapped_args = &DMValue::from(List::from_iter(converted));
            wrapper_proc.call(&[&DMValue::from_string(proc_name).unwrap(), wrapped_args])
        }
        // If not, directly call the proc
        None => {
            // So that both "/proc/proc_name" and "proc_name" are valid syntaxes
            if !proc_name.starts_with("/proc/") {
                proc_name.insert_str(0, "/proc/");
            }
            let proc =
                Proc::find(proc_name.clone()).ok_or_else(|| runtime!("{} not found", proc_name))?;
            proc.call(converted.iter().collect::<Vec<&DMValue>>().as_slice())
        }
    });
    match call_result {
        Ok(ret) => Value::try_from(&ret).map_err(|e| external!(e.message)),
        Err(e) => Err(external!(e.message)),
    }
}

#[derive(Clone)]
struct GenericWrapper {
    pub value: DMValue,
}

impl From<GenericWrapper> for DMValue {
    fn from(wrapper: GenericWrapper) -> Self {
        wrapper.value
    }
}

impl<T> PartialEq<T> for GenericWrapper
where
    T: Clone + Into<DMValue>,
{
    fn eq(&self, other: &T) -> bool {
        self.value == other.clone().into()
    }
}

impl UserData for GenericWrapper {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(mlua::MetaMethod::Eq, datum_equality);
        methods.add_meta_function(mlua::MetaMethod::ToString, datum_to_string);
    }
}

#[derive(Clone)]
pub enum Value {
    Null,
    Number(f32),
    String(String),
    ListRef(DMValue),
    List(Vec<(Self, Self)>),
    Datum(WeakValue),
    Global(DMValue),
    Other(DMValue),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Null => matches!(other, Self::Null),
            Self::Number(n) => matches!(other, Self::Number(n2) if n == n2),
            Self::String(s) => matches!(other, Self::String(s2) if s == s2),
            Self::ListRef(l) => match other {
                Self::ListRef(l2) => l == l2,
                Self::Datum(w) => l == &w.upgrade_or_null(),
                Self::Global(g) => l == g,
                Self::Other(o) => l == o,
                _ => false,
            },
            Self::List(l) => match other {
                Self::List(l2) => l == l2,
                _ => false,
            },
            Self::Datum(w) => match other {
                Self::ListRef(l) => &w.upgrade_or_null() == l,
                Self::Datum(w2) => w.upgrade_or_null() == w2.upgrade_or_null(),
                Self::Global(g) => &w.upgrade_or_null() == g,
                Self::Other(o) => &w.upgrade_or_null() == o,
                _ => false,
            },
            Self::Global(g) => match other {
                Self::ListRef(l) => g == l,
                Self::Datum(w) => g == &w.upgrade_or_null(),
                Self::Global(g2) => g == g2,
                Self::Other(o) => g == o,
                _ => false,
            },
            Self::Other(o) => match other {
                Self::ListRef(l) => o == l,
                Self::Datum(w) => o == &w.upgrade_or_null(),
                Self::Global(g) => o == g,
                Self::Other(o2) => o == o2,
                _ => false,
            },
        }
    }
}

impl TryFrom<&DMValue> for Value {
    type Error = Runtime;
    fn try_from(value: &DMValue) -> Result<Self, Self::Error> {
        match value.raw.tag {
            ValueTag::Null => Ok(Self::Null),
            ValueTag::Number => Ok(Self::Number(value.as_number()?)),
            ValueTag::String => Ok(Self::String(value.as_string()?)),
            ValueTag::List
            | ValueTag::MobVars
            | ValueTag::ObjVars
            | ValueTag::TurfVars
            | ValueTag::AreaVars
            | ValueTag::ClientVars
            | ValueTag::Vars
            | ValueTag::MobOverlays
            | ValueTag::MobUnderlays
            | ValueTag::ObjOverlays
            | ValueTag::ObjUnderlays
            | ValueTag::TurfOverlays
            | ValueTag::TurfUnderlays
            | ValueTag::AreaOverlays
            | ValueTag::AreaUnderlays
            | ValueTag::ImageVars
            | ValueTag::WorldVars
            | ValueTag::GlobalVars => Ok(Self::ListRef(value.clone())),
            ValueTag::Datum
            | ValueTag::Area
            | ValueTag::Turf
            | ValueTag::Obj
            | ValueTag::Mob
            | ValueTag::Client
            | ValueTag::Image => Ok(Self::Datum(value.as_weak()?)),
            ValueTag::World => Ok(Self::Global(value.clone())),
            _ => Ok(Self::Other(value.clone())),
        }
    }
}

impl TryFrom<Value> for DMValue {
    type Error = Runtime;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Null => Ok(DMValue::null()),
            Value::Number(n) => Ok(DMValue::from(n)),
            Value::String(s) => DMValue::from_string(s),
            Value::ListRef(l) => Ok(l),
            Value::List(vec) => {
                let len = vec.clone().into_iter().fold(0, |max, elem| {
                    let (key, _) = elem;
                    if let Value::Number(n) = key {
                        max.max(n as u32)
                    } else {
                        max
                    }
                });
                let list = List::with_size(len);
                for (key, value) in vec.into_iter() {
                    list.set(Self::try_from(key)?, Self::try_from(value)?)?;
                }
                Ok(DMValue::from(list))
            }
            Value::Datum(weak) => Ok(weak.upgrade_or_null()),
            Value::Global(glob) => Ok(glob),
            Value::Other(other) => Ok(other),
        }
    }
}

impl<'lua> ToLua<'lua> for Value {
    fn to_lua(self, lua: &mlua::Lua) -> mlua::Result<MluaValue> {
        match self {
            Self::Null => Ok(mlua::Nil),
            Self::Number(n) => Ok(MluaValue::Number(n as f64)),
            Self::String(s) => Ok(MluaValue::String(lua.create_string(&s)?)),
            Self::ListRef(l) => Ok(MluaValue::UserData(
                lua.create_userdata(ListWrapper { value: l })?,
            )),
            Self::List(vec) => {
                let table = lua.create_table()?;
                for (key, value) in vec.into_iter() {
                    table.raw_set(key, value)?;
                }
                Ok(MluaValue::Table(table))
            }
            Self::Datum(weak) => Ok(MluaValue::UserData(
                lua.create_userdata(DatumWrapper { value: weak })?,
            )),
            Self::Global(glob) => Ok(MluaValue::UserData(
                lua.create_userdata(GlobalWrapper::new(glob))?,
            )),
            Self::Other(other) => Ok(MluaValue::UserData(
                lua.create_userdata(GenericWrapper { value: other })?,
            )),
        }
    }
}

impl<'lua> FromLua<'lua> for Value {
    fn from_lua(value: MluaValue, lua: &mlua::Lua) -> mlua::Result<Self> {
        let typename = value.type_name();
        let pointer = value.to_pointer();
        match value {
            MluaValue::Nil => Ok(Self::Null),
            MluaValue::Boolean(b) => Ok(Self::Number(if b { 1.0 } else { 0.0 })),
            MluaValue::Integer(i) => Ok(Self::Number(i as f32)),
            MluaValue::Number(n) => Ok(Self::Number(n as f32)),
            MluaValue::String(s) => Ok(Self::String(String::from(s.to_str()?))),
            MluaValue::Table(t) => {
                let mut vec: Vec<(Self, Self)> = Vec::new();
                for pair in t.pairs() {
                    let (key, value): (MluaValue, MluaValue) = pair?;
                    vec.push((Self::from_lua(key, lua)?, Self::from_lua(value, lua)?))
                }
                Ok(Self::List(vec))
            }
            MluaValue::UserData(ud) => {
                if let Ok(list) = ud.borrow::<ListWrapper>() {
                    Ok(Self::ListRef(list.value.clone()))
                } else if let Ok(datum) = ud.borrow::<DatumWrapper>() {
                    Ok(Self::Datum(datum.value))
                } else if let Ok(global) = ud.borrow::<GlobalWrapper>() {
                    Ok(Self::Global(global.value.clone()))
                } else if let Ok(generic) = ud.borrow::<GenericWrapper>() {
                    Ok(Self::Other(generic.value.clone()))
                } else {
                    Ok(Self::String(format!("{typename}: {pointer:p}")))
                }
            }
            _ => Ok(Self::String(format!("{typename}: {pointer:p}"))),
        }
    }
}
