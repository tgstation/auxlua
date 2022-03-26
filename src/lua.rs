use auxtools::{List, Proc, WeakValue, StringRef, DMResult, Runtime, runtime, raw_types::values::ValueTag};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::error::Error;
use std::convert::{TryFrom};
use std::iter::FromIterator;
use mlua::{Lua, FromLua, ToLua, UserData, UserDataFields, UserDataMethods, MultiValue};
use std::cell::RefCell;
use std::time::Instant;

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
    ($fmt:expr) => {
        mlua::Error::external(AuxluaError::new($fmt))
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

thread_local!{
    pub static LUA_THREAD_START: RefCell<Instant> = RefCell::new(Instant::now());
    pub static SET_VAR_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
    pub static DATUM_CALL_PROC_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
    pub static GLOBAL_CALL_PROC_WRAPPER: RefCell<Option<String>> = RefCell::new(None);
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
    // This should take strings directly, rather than hiding the potentially expensive allocations
    pub fn new<T>(s: T) -> Self 
    where T: Into<String> {
        Self(s.into())
    }
}

#[derive(Clone)]
pub struct ListWrapper {
    pub value: auxtools::Value
}

impl From<ListWrapper> for auxtools::Value {
    fn from(wrapper: ListWrapper) -> Self {
        wrapper.value
    }
}

impl<T> PartialEq<T> for ListWrapper
where T: Clone + Into<auxtools::Value> {
    fn eq(&self, other: &T) -> bool {
        self.value == other.clone().into()
    }
}

impl UserData for ListWrapper {
    fn add_fields<'lua, F: UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("len", |_, this| {
            this.value.as_list().map_err(|_| external!("not a list"))
            .and_then(|list| Ok(list.len()))
        });
    }

    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(mlua::MetaMethod::Eq, datum_equality);
        
        methods.add_method("get", |_, this, key: Value| {
            this.value.as_list().map_err(|_| runtime!("not a list"))
            .and_then(|list| auxtools::Value::try_from(key).and_then(|key_val|
                list.get(key_val).and_then(|value| Value::try_from(&value))))
            .map_err(|e| external!(e.message))
        });

        methods.add_method("set", |_, this, args: (Value, Value)| {
            // Just `(key, value): (Value, Value)` at the above call
            let (key, value) = args;
            this.value.as_list().map_err(|_| runtime!("not a list"))
            .and_then(|list| auxtools::Value::try_from(key).and_then(|key_val|
                list.set(key_val, auxtools::Value::try_from(value)?)))
            .map_err(|e| external!(e.message))
        });

        methods.add_method("add", |_, this, elem: Value| {
            this.value.as_list().map_err(|_| runtime!("not a list"))
            .and_then(|list| auxtools::Value::try_from(elem).and_then(|elem_val| {
                list.append(elem_val);
                Ok(mlua::Nil)
            }))
            .map_err(|e| external!(e.message))
        });

        methods.add_method("to_table", |_, this, _: MultiValue| {
            this.value.as_list().map_err(|_| runtime!("not a list"))
            .and_then(|list| tablify_list(list)).map_err(|e| external!(e.message))
        });
    }
}

pub fn tablify_list(list: auxtools::List) -> DMResult<Value> {
    let mut vec: Vec<(Value, Value)> = Vec::new();
    for i in 1..=list.len() {
        // Does mlua not provide a basic iterator or similar?
        let key = &list.get(i)?;
        if key.raw.tag == ValueTag::Null {
            continue
        }
        let list_value = &list.get(key).unwrap_or(auxtools::Value::null());
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

impl From<DatumWrapper> for auxtools::Value {
    fn from(wrapper: DatumWrapper) -> Self {
        wrapper.value.upgrade_or_null()
    }
}

impl<T> PartialEq<T> for DatumWrapper
where T: Clone + Into<auxtools::Value> {
    fn eq(&self, other: &T) -> bool {
        self.value.upgrade_or_null() == other.clone().into()
    }
}

fn datum_call_proc<'lua>(lua: &'lua Lua, datum: &auxtools::Value, args: MultiValue<'lua>) -> mlua::Result<Value> {
    let mut args_iter = args.into_iter();
    let first_arg: mlua::Value<'lua> = args_iter.next()
    .ok_or(external!("attempted call with 0 arguments"))?; // For this error to be more useful, explain what's expected that wasn't (I'm assuming the self datum).
    let proc = String::from_lua(first_arg, lua)?;
    let proc_args = args_iter.try_fold::<_, _, mlua::Result<_>>(Vec::<Value>::new(), |mut acc, val| {
        let arg = Value::from_lua(val, lua)?;
        // Why are you using fold here instead of just collect?
        acc.push(arg);
        Ok(acc)
    })?;
    let converted: Vec<auxtools::Value> = proc_args.into_iter()
    .map(|v| auxtools::Value::try_from(v)).collect::<Result<Vec<auxtools::Value>, Runtime>>()
    .map_err(|e| external!(e.message))?;
    let call_result = DATUM_CALL_PROC_WRAPPER.with(|wrapper| match &*wrapper.borrow() {
        Some(wrapper_name) => {
            let wrapper_proc = Proc::find(wrapper_name).ok_or(specific_runtime!("{} not found", wrapper_name))?;
            let wrapped_args = &auxtools::Value::from(List::from_iter(converted));
            wrapper_proc.call(&[datum, &auxtools::Value::from_string(proc).unwrap(), wrapped_args])
        }
        None => datum.call(proc, converted.iter().collect::<Vec<&auxtools::Value>>().as_slice())
    });
    match call_result {
        Ok(ret) => Value::try_from(&ret).map_err(|e| external!(e.message)),
        Err(e) => Err(external!(e.message))
    }
}

fn datum_set_var(datum: &auxtools::Value, var: String, value: Value) -> DMResult<()> {
    auxtools::Value::from_string(var).and_then(|var_string| {
        let value = &auxtools::Value::try_from(value)?;
        SET_VAR_WRAPPER.with(|wrapper| match &*wrapper.borrow() {
            Some(wrapper_proc) => {
                match auxtools::Value::globals().call(wrapper_proc, &[datum, &var_string, value]) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(specific_runtime!(e.message))
                }
            },
            None => datum.set(StringRef::from_value(var_string).unwrap(), value)
        })
    })
}

impl UserData for DatumWrapper {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(mlua::MetaMethod::Eq, datum_equality);
        
        methods.add_method("get_var", |_, this, var: String| {
            this.value.upgrade().ok_or(runtime!("datum was deleted"))
            .and_then(|datum| StringRef::from_raw(var.as_bytes()).and_then(|var_ref|
                datum.get(var_ref).and_then(|value| Value::try_from(&value))))
            .map_err(|e| external!(e.message))
        });

        methods.add_method("set_var", |_, this, args: (String, Value)| {
            let (var, value) = args;
            this.value.upgrade().ok_or(runtime!("datum was deleted"))
            .and_then(|datum| datum_set_var(&datum, var, value))
            .map_err(|e| external!(e.message))
        });

        methods.add_method("call_proc", |lua, this, args: MultiValue| {
            let datum = this.value.upgrade().ok_or(external!("datum was deleted"))?;
            datum_call_proc(lua, &datum, args)
        });
    }
}

impl DatumWrapper {
    pub fn new(value: &auxtools::Value) -> mlua::Result<Self> {
        value.as_weak().map_err(|_| mlua::Error::FromLuaConversionError {
            from: "datum",
            to: "userdata",
            message: Some(String::from("weak value creation failed")),
        }).and_then(|weak_val| Ok(Self { value: weak_val }))
    }
}

#[derive(Clone)]
pub struct GlobalWrapper {
    pub value: auxtools::Value
}

impl From<GlobalWrapper> for auxtools::Value {
    fn from(wrapper: GlobalWrapper) -> Self {
        wrapper.value
    }
}

impl<T> PartialEq<T> for GlobalWrapper
where T: Clone + Into<auxtools::Value> {
    fn eq(&self, other: &T) -> bool {
        self.value == other.clone().into()
    }
}

fn datum_equality<'lua>(_: &'lua Lua, args: (Value, Value)) -> mlua::Result<bool> {
    Ok(args.0 == args.1)
}

impl UserData for GlobalWrapper {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(mlua::MetaMethod::Eq, datum_equality);

        methods.add_method("get_var", |_, this, var: String| {
            StringRef::from_raw(var.as_bytes()).and_then(|var_ref|
                this.value.get(var_ref).and_then(|value| Value::try_from(&value)))
            .map_err(|e| external!(e.message))
        });

        methods.add_method("set_var", |_, this, args: (String, Value)| {
            let (var, value) = args;
            datum_set_var(&this.value, var, value)
            .map_err(|e| external!(e.message))
        });

        methods.add_method("call_proc", |lua, this, args: MultiValue| {
            datum_call_proc(lua, &this.value, args)
        });
    }
}

impl GlobalWrapper {
    pub fn new(value: auxtools::Value) -> Self {
        Self { value: value }
    }
}

pub fn global_proc_call<'a>(lua: &'a mlua::Lua, args: MultiValue<'a>) -> mlua::Result<Value>{
    let mut args_iter = args.into_iter();
    let first_arg: mlua::Value<'a> = args_iter.next()
    .ok_or(external!("attempted call with 0 arguments"))?;
    let mut proc_name = String::from_lua(first_arg, lua)?;
    // Same here, use collect
    let proc_args = args_iter.try_fold::<_, _, mlua::Result<_>>(Vec::<Value>::new(), |mut acc, val| {
        let arg = Value::from_lua(val, lua)?;
        acc.push(arg);
        Ok(acc)
    })?;
    let converted: Vec<auxtools::Value> = proc_args.into_iter()
    .map(|v| auxtools::Value::try_from(v)).collect::<Result<Vec<auxtools::Value>, Runtime>>()
    .map_err(|e| external!(e.message))?;
    let call_result = GLOBAL_CALL_PROC_WRAPPER.with(|wrapper| match &*wrapper.borrow() {
        Some(wrapper_name) => {
            let wrapper_proc = Proc::find(wrapper_name).ok_or(specific_runtime!("{} not found", wrapper_name))?;
            let wrapped_args = &auxtools::Value::from(List::from_iter(converted));
            wrapper_proc.call(&[&auxtools::Value::from_string(proc_name).unwrap(), wrapped_args])
        }
        None => {
            if !proc_name.starts_with("/proc/") {
                proc_name.insert_str(0, "/proc/");
            }
            let proc = Proc::find(proc_name.clone())
            .ok_or(runtime!("{} not found", proc_name))?;
            proc.call(converted.iter().collect::<Vec<&auxtools::Value>>().as_slice())
        }
    });
    match call_result {
        Ok(ret) => Value::try_from(&ret).map_err(|e| external!(e.message)),
        Err(e) => Err(external!(e.message))
    }
}

#[derive(Clone)]
struct GenericWrapper {
    pub value: auxtools::Value
}

impl From<GenericWrapper> for auxtools::Value {
    fn from(wrapper: GenericWrapper) -> Self {
        wrapper.value
    }
}

impl<T> PartialEq<T> for GenericWrapper
where T: Clone + Into<auxtools::Value> {
    fn eq(&self, other: &T) -> bool {
        self.value == other.clone().into()
    }
}

impl UserData for GenericWrapper {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(mlua::MetaMethod::Eq, datum_equality);
    }
}

#[derive(Clone)]
pub enum Value {
    Null,
    Number(f32),
    String(String),
    ListRef(auxtools::Value),
    List(Vec<(Self, Self)>),
    Datum(WeakValue),
    Global(auxtools::Value),
    Other(auxtools::Value),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Null => {
                match other {
                    // Why not just `other == Self::Null`?
                    Self::Null => true,
                    _ => false
                }
            },
            // Don't use these one letter variables everywhere. Write them out completely.
            Self::Number(n) => {
                match other {
                    Self::Number(n2) => n == n2,
                    _ => false
                }
            },
            Self::String(s) => {
                match other {
                    Self::String(s2) => s == s2,
                    _ => false
                }
            },
            Self::ListRef(l) => {
                match other {
                    Self::ListRef(l2) => l == l2,
                    Self::Datum(w) => l.clone() == w.upgrade_or_null(),
                    Self::Global(g) => l == g,
                    Self::Other(o) => l == o,
                    _ => false
                }
            },
            Self::List(l) => {
                match other {
                    Self::List(l2) => l == l2,
                    _ => false
                }
            },
            Self::Datum(w) => {
                match other {
                    // Why so much cloning?
                    Self::ListRef(l) => w.upgrade_or_null() == l.clone(),
                    Self::Datum(w2) => w.upgrade_or_null() == w2.upgrade_or_null(),
                    Self::Global(g) => w.upgrade_or_null() == g.clone(),
                    Self::Other(o) => w.upgrade_or_null() == o.clone(),
                    _ => false
                }
            },
            Self::Global(g) => {
                match other {
                    Self::ListRef(l) => g == l,
                    Self::Datum(w) => g.clone() == w.upgrade_or_null(),
                    Self::Global(g2) => g == g2,
                    Self::Other(o) => g == o,
                    _ => false
                }
            },
            Self::Other(o) => {
                match other {
                    Self::ListRef(l) => o == l,
                    Self::Datum(w) => o.clone() == w.upgrade_or_null(),
                    Self::Global(g) => o == g,
                    Self::Other(o2) => o == o2,
                    _ => false
                }
            }
        }
    }
}

impl TryFrom<&auxtools::Value> for Value {
    type Error = Runtime;
    fn try_from(value: &auxtools::Value) -> Result<Self, Self::Error> {
        match value.raw.tag {
            ValueTag::Null => Ok(Self::Null),
            ValueTag::Number => Ok(Self::Number(value.as_number()?)),
            ValueTag::String => Ok(Self::String(value.as_string()?)),
            ValueTag::List => Ok(Self::ListRef(value.clone())),
            ValueTag::Datum | ValueTag::Area | ValueTag::Turf
            | ValueTag::Obj | ValueTag::Mob | ValueTag::Client
            | ValueTag::Image => {
                Ok(Self::Datum(value.as_weak()?))
            },
            ValueTag::World => Ok(Self::Global(value.clone())),
            _ => Ok(Self::Other(value.clone()))
        }
    }
}

impl TryFrom<Value> for auxtools::Value {
    type Error = Runtime;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Null => Ok(auxtools::Value::null()),
            Value::Number(n) => Ok(auxtools::Value::from(n)),
            Value::String(s) => auxtools::Value::from_string(s),
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
                Ok(auxtools::Value::from(list))
            },
            Value::Datum(weak) => Ok(weak.upgrade_or_null()),
            Value::Global(glob) => Ok(glob),
            Value::Other(other) => Ok(other),
        }
    }
}

impl<'lua> ToLua<'lua> for Value {
    fn to_lua(self, lua: &mlua::Lua) -> mlua::Result<mlua::Value> {
        match self {
            Self::Null => Ok(mlua::Nil),
            Self::Number(n) => Ok(mlua::Value::Number(n as f64)),
            Self::String(s) => Ok(mlua::Value::String(lua.create_string(&s)?)),
            Self::ListRef(l) => Ok(mlua::Value::UserData(lua.create_userdata(ListWrapper{value:l})?)),
            Self::List(vec) => {
                let table = lua.create_table()?;
                for (key, value) in vec.into_iter() {
                    table.raw_set(key, value)?;
                }
                Ok(mlua::Value::Table(table))
            },
            Self::Datum(weak) => Ok(mlua::Value::UserData(lua.create_userdata(DatumWrapper{value:weak})?)),
            Self::Global(glob) => Ok(mlua::Value::UserData(lua.create_userdata(GlobalWrapper::new(glob))?)),
            Self::Other(other) => Ok(mlua::Value::UserData(lua.create_userdata(GenericWrapper{value:other})?))
        }
    }
}

impl<'lua> FromLua<'lua> for Value {
    fn from_lua(value: mlua::Value, lua: &mlua::Lua) -> mlua::Result<Self> {
        let unsupported_value: mlua::Error = mlua::Error::ToLuaConversionError {
            from: value.type_name(),
            to: "BYOND value",
            message: Some(String::from("unsupported value type"))
        };
        match value {
            mlua::Value::Nil => Ok(Self::Null),
            // 1 if true, 2 if false???
            mlua::Value::Boolean(b) => Ok(Self::Number(if b {1.0} else {2.0})),
            mlua::Value::Integer(i) => Ok(Self::Number(i as f32)),
            mlua::Value::Number(n) => Ok(Self::Number(n as f32)),
            mlua::Value::String(s) => Ok(Self::String(String::from(s.to_str()?))),
            mlua::Value::Table(t) => {
                let mut vec: Vec<(Self, Self)> = Vec::new();
                for pair in t.pairs() {
                    let (key, value): (mlua::Value, mlua::Value) = pair?;
                    vec.push((Self::from_lua(key, lua)?, Self::from_lua(value, lua)?))
                }
                Ok(Self::List(vec))
            }, mlua::Value::UserData(ud) => {
                if let Ok(list) = ud.borrow::<ListWrapper>() {
                    Ok(Self::ListRef(list.value.clone()))
                } else if let Ok(datum) = ud.borrow::<DatumWrapper>() {
                    Ok(Self::Datum(datum.value))
                } else if let Ok(global) = ud.borrow::<GlobalWrapper>() {
                    Ok(Self::Global(global.value.clone()))
                } else if let Ok(generic) = ud.borrow::<GenericWrapper>() {
                    Ok(Self::Other(generic.value.clone()))
                } else {
                    Err(unsupported_value)
                }
            },
            _ => Err(unsupported_value)
        }
    }
}

