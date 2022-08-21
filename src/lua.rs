use auxtools::{
    raw_types::values::ValueTag, runtime, DMResult, List, Proc, Runtime, StringRef, WeakValue,
};
use mlua::{
    AnyUserData, Error::ToLuaConversionError, FromLua, Function, Lua, MetaMethod, MultiValue,
    Table, ToLua, UserData, UserDataFields, UserDataMethods,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::os::raw::c_void;
use std::rc::Rc;
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

trait TryIntoValue {
    fn try_into_value(&self) -> mlua::Result<DMValue>;
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

impl TryIntoValue for ListWrapper {
    fn try_into_value(&self) -> mlua::Result<DMValue> {
        Ok(self.value.clone())
    }
}

#[derive(Clone)]
pub struct ListIndexer {
    pub value: DMValue,
}

impl From<ListIndexer> for DMValue {
    fn from(wrapper: ListIndexer) -> Self {
        wrapper.value
    }
}

impl<T> PartialEq<T> for ListIndexer
where
    T: Clone + Into<DMValue>,
{
    fn eq(&self, other: &T) -> bool {
        self.value == other.clone().into()
    }
}

impl TryIntoValue for ListIndexer {
    fn try_into_value(&self) -> mlua::Result<DMValue> {
        Ok(self.value.clone())
    }
}

#[derive(Clone)]
pub struct DatumTiedList {
    pub parent_value: WeakValue,
    pub value: DMValue,
}

impl From<DatumTiedList> for DMValue {
    fn from(list: DatumTiedList) -> Self {
        if list.parent_value.upgrade().is_some() {
            list.value
        } else {
            DMValue::null()
        }
    }
}

impl<T> PartialEq<T> for DatumTiedList
where
    T: Clone + Into<DMValue>,
{
    #[allow(clippy::cmp_owned)]
    fn eq(&self, other: &T) -> bool {
        DMValue::from(self.clone()) == other.clone().into()
    }
}

impl TryIntoValue for DatumTiedList {
    fn try_into_value(&self) -> mlua::Result<DMValue> {
        if self.parent_value.upgrade().is_some() {
            Ok(self.value.clone())
        } else {
            Err(external!("list tied to deleted datum"))
        }
    }
}

#[derive(Clone)]
pub struct DatumTiedListIndexer {
    pub parent_value: WeakValue,
    pub value: DMValue,
}

impl From<DatumTiedListIndexer> for DMValue {
    fn from(list: DatumTiedListIndexer) -> Self {
        if list.parent_value.upgrade().is_some() {
            list.value
        } else {
            DMValue::null()
        }
    }
}

impl<T> PartialEq<T> for DatumTiedListIndexer
where
    T: Clone + Into<DMValue>,
{
    #[allow(clippy::cmp_owned)]
    fn eq(&self, other: &T) -> bool {
        DMValue::from(self.clone()) == other.clone().into()
    }
}

impl TryIntoValue for DatumTiedListIndexer {
    fn try_into_value(&self) -> mlua::Result<DMValue> {
        if self.parent_value.upgrade().is_some() {
            Ok(self.value.clone())
        } else {
            Err(external!("list tied to deleted datum"))
        }
    }
}

fn list_len<T>(_: &Lua, list: &T) -> mlua::Result<u32>
where
    T: TryIntoValue,
{
    list.try_into_value()?
        .as_list()
        .map_err(|_| external!("not a list"))
        .map(|list| list.len())
}

fn list_get<T>(_: &Lua, list: &T, key: Value) -> mlua::Result<Value>
where
    T: TryIntoValue,
{
    list.try_into_value()?
        .as_list()
        .map_err(|_| runtime!("not a list"))
        .and_then(|list| {
            DMValue::try_from(key)
                .and_then(|key_val| list.get(key_val).and_then(|value| Value::try_from(&value)))
        })
        .map_err(|e| external!(e.message))
}

fn list_set<T>(_: &Lua, list: &T, (key, value): (Value, Value)) -> mlua::Result<()>
where
    T: TryIntoValue,
{
    let list = list.try_into_value()?;
    match list.raw.tag {
        ValueTag::MobVars
        | ValueTag::ObjVars
        | ValueTag::TurfVars
        | ValueTag::AreaVars
        | ValueTag::ClientVars
        | ValueTag::Vars
        | ValueTag::ImageVars
        | ValueTag::WorldVars
        | ValueTag::GlobalVars => {
            if SET_VAR_WRAPPER.with(|wrapper| wrapper.borrow().is_some()) {
                return Err(external!(
                    "Cannot modify vars lists when a var-setting wrapper proc is set."
                ));
            }
        }
        ValueTag::MobContents
        | ValueTag::TurfContents
        | ValueTag::AreaContents
        | ValueTag::ObjContents => {
            return Err(external!(
                "Cannot directly modify contents lists. Set the elements' \"loc\" var instead."
            ))
        }
        ValueTag::TurfVisContents
        | ValueTag::ObjVisContents
        | ValueTag::MobVisContents
        | ValueTag::ImageVisContents => {
            return Err(external!(
                "Cannot modify vis_contents lists by index. Use \"add\" or \"remove\" instead."
            ))
        }
        ValueTag::TurfVisLocs | ValueTag::ObjVisLocs | ValueTag::MobVisLocs => {
            return Err(external!("Cannot modify vis_locs lists."))
        }
        _ => (),
    };
    list.as_list()
        .map_err(|_| runtime!("not a list"))
        .and_then(|list| {
            DMValue::try_from(key).and_then(|key_val| list.set(key_val, DMValue::try_from(value)?))
        })
        .map_err(|e| external!(e.message))
}

fn list_add<T>(_: &Lua, list: &T, elem: Value) -> mlua::Result<()>
where
    T: TryIntoValue,
{
    let list = list.try_into_value()?;
    match list.raw.tag {
        ValueTag::MobVars
        | ValueTag::ObjVars
        | ValueTag::TurfVars
        | ValueTag::AreaVars
        | ValueTag::ClientVars
        | ValueTag::Vars
        | ValueTag::ImageVars
        | ValueTag::WorldVars
        | ValueTag::GlobalVars => return Err(external!("Cannot add to vars lists")),
        ValueTag::MobContents
        | ValueTag::TurfContents
        | ValueTag::AreaContents
        | ValueTag::ObjContents => {
            return Err(external!(
                "Cannot directly modify contents lists. Set the elements' \"loc\" var instead."
            ))
        }
        ValueTag::ArgList => return Err(external!("Cannot add to args lists")),
        ValueTag::TurfVisContents
        | ValueTag::ObjVisContents
        | ValueTag::MobVisContents
        | ValueTag::ImageVisContents => {
            let thing: DMValue =
                DMValue::try_from(elem.clone()).map_err(|e| external!(e.message))?;
            match thing.raw.tag {
                ValueTag::Obj | ValueTag::Turf | ValueTag::Mob | ValueTag::Image => (),
                _ => {
                    return Err(external!(
                        "vis_contents lists only accept turfs and movable atoms."
                    ))
                }
            }
        }
        ValueTag::TurfVisLocs | ValueTag::ObjVisLocs | ValueTag::MobVisLocs => {
            return Err(external!("Cannot modify vis_locs lists."))
        }
        _ => (),
    };
    list.as_list()
        .map_err(|_| runtime!("not a list"))
        .and_then(|list| DMValue::try_from(elem).map(|elem_val| list.append(elem_val)))
        .map_err(|e| external!(e.message))
}

fn list_remove<T>(_: &Lua, list: &T, elem: Value) -> mlua::Result<()>
where
    T: TryIntoValue,
{
    let list = list.try_into_value()?;
    match list.raw.tag {
        ValueTag::MobVars
        | ValueTag::ObjVars
        | ValueTag::TurfVars
        | ValueTag::AreaVars
        | ValueTag::ClientVars
        | ValueTag::Vars
        | ValueTag::ImageVars
        | ValueTag::WorldVars
        | ValueTag::GlobalVars => return Err(external!("Cannot remove from vars-type lists")),
        ValueTag::ArgList => return Err(external!("Cannot remove from args lists")),
        ValueTag::MobContents
        | ValueTag::TurfContents
        | ValueTag::AreaContents
        | ValueTag::ObjContents => {
            return Err(external!(
                "Cannot directly modify contents lists. Set the elements' \"loc\" var instead."
            ))
        }
        ValueTag::TurfVisLocs | ValueTag::ObjVisLocs | ValueTag::MobVisLocs => {
            return Err(external!("Cannot modify vis_locs lists."))
        }
        _ => (),
    };
    list.as_list()
        .map_err(|_| runtime!("not a list"))
        .and_then(|list| DMValue::try_from(elem).map(|elem_val| list.remove(elem_val)))
        .map_err(|e| external!(e.message))
}

fn list_to_table<T>(_: &Lua, list: &T, _: ()) -> mlua::Result<Value>
where
    T: TryIntoValue,
{
    list.try_into_value()?
        .as_list()
        .map_err(|_| runtime!("not a list"))
        .and_then(tablify_list)
        .map_err(|e| external!(e.message))
}

fn list_of_type<T>(_: &Lua, list: &T, type_path: String) -> mlua::Result<Value>
where
    T: TryIntoValue,
{
    if !type_path.starts_with('/') {
        return Err(external!("type path must start with '/'"));
    }

    let filter_type_split = type_path.split('/').collect::<Vec<_>>();

    list.try_into_value()?
        .as_list()
        .map_err(|_| runtime!("not a list"))
        .and_then(tablify_list)
        .map(|value| {
            let table = match value {
                Value::List(list) => list,
                _ => unreachable!("tablify_list did not return a table"),
            };

            let filtered_table = table
                .borrow()
                .iter()
                .filter(|(_, value)| {
                    let weak_datum = match value {
                        Value::Datum(weak_datum) => weak_datum,
                        _ => return false,
                    };

                    let datum = match weak_datum.upgrade() {
                        Some(datum) => datum,
                        None => return false,
                    };

                    let datum_type = match datum.get_type() {
                        Ok(datum_type) => datum_type,
                        Err(_) => return false,
                    };

                    // Support :of_type("/mob") with types like "/mob/living/carbon/human"
                    let type_split = datum_type.split('/');

                    type_split
                        .zip(&filter_type_split)
                        .all(|(type_part, filter_type_part)| type_part == *filter_type_part)
                })
                .cloned()
                .collect();

            Value::List(Rc::new(RefCell::new(filtered_table)))
        })
        .map_err(|e| external!(e.message))
}

fn list_iter<'lua, T>(lua: &'lua Lua, list: &T, _: ()) -> mlua::Result<(Function<'lua>, Value)>
where
    T: TryIntoValue,
{
    Ok((
        lua.globals().get::<_, mlua::Function>("next")?,
        tablify_list(
            list.try_into_value()?
                .as_list()
                .map_err(|_| external!("not a list"))?,
        )
        .map_err(|e| external!(e.message))?,
    ))
}

impl UserData for ListWrapper {
    fn add_fields<'lua, F: UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("len", list_len);

        fields.add_field_method_get("entries", |_, this: &ListWrapper| {
            Ok(Value::ListIndexer(this.value.clone()))
        });
    }

    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(MetaMethod::Eq, datum_equality);
        methods.add_meta_function(MetaMethod::ToString, datum_to_string);
        methods.add_function("is_null", datum_truthiness);
        methods.add_meta_method(MetaMethod::Len, |lua, this, ()| list_len(lua, this));

        methods.add_method("get", list_get);

        methods.add_method("set", list_set);

        methods.add_method("add", list_add);

        methods.add_method("remove", list_remove);

        methods.add_method("to_table", list_to_table);

        methods.add_method("of_type", list_of_type);

        methods.add_meta_method(MetaMethod::Iter, list_iter);
    }
}

impl UserData for ListIndexer {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Index, list_get);

        methods.add_meta_method(MetaMethod::NewIndex, list_set);
    }
}

impl UserData for DatumTiedList {
    fn add_fields<'lua, F: UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("len", list_len);

        fields.add_field_method_get("entries", |_, this: &DatumTiedList| {
            Ok(Value::DatumListIndexer(
                this.parent_value,
                this.value.clone(),
            ))
        });
    }

    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(MetaMethod::Eq, datum_equality);
        methods.add_meta_function(MetaMethod::ToString, datum_to_string);
        methods.add_function("is_null", datum_truthiness);
        methods.add_meta_method(MetaMethod::Len, |lua, this, ()| list_len(lua, this));

        methods.add_method("get", list_get);

        methods.add_method("set", list_set);

        methods.add_method("add", list_add);

        methods.add_method("remove", list_remove);

        methods.add_method("to_table", list_to_table);

        methods.add_method("of_type", list_of_type);

        methods.add_meta_method(MetaMethod::Iter, list_iter);
    }
}

impl UserData for DatumTiedListIndexer {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Index, list_get);

        methods.add_meta_method(MetaMethod::NewIndex, list_set);
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
    Ok(Value::List(Rc::new(RefCell::new(vec))))
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

#[derive(Copy, Clone)]
pub struct DatumIndexer {
    pub value: WeakValue,
}

impl From<DatumIndexer> for DMValue {
    fn from(wrapper: DatumIndexer) -> Self {
        wrapper.value.upgrade_or_null()
    }
}

impl<T> PartialEq<T> for DatumIndexer
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

fn datum_get_var(datum: &DMValue, var: String) -> DMResult<Value> {
    StringRef::from_raw(var.as_bytes()).and_then(|var_ref| {
        datum.get(var_ref).and_then(|value| match value.raw.tag {
            ValueTag::MobContents
            | ValueTag::TurfContents
            | ValueTag::AreaContents
            | ValueTag::ObjContents
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
            | ValueTag::ImageOverlays
            | ValueTag::ImageUnderlays
            | ValueTag::TurfVisContents
            | ValueTag::ObjVisContents
            | ValueTag::MobVisContents
            | ValueTag::ImageVisContents
            | ValueTag::TurfVisLocs
            | ValueTag::ObjVisLocs
            | ValueTag::MobVisLocs
            | ValueTag::ImageVars => Ok(Value::DatumList(datum.as_weak()?, value)),
            _ => Value::try_from(&value),
        })
    })
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

fn datum_truthiness(_: &Lua, arg: Value) -> mlua::Result<bool> {
    Ok(match arg {
        Value::Null => true,
        Value::Number(n) => n == 0.0,
        Value::String(s) => s.is_empty(),
        Value::Datum(weak) => weak.upgrade().is_none(),
        Value::DatumList(weak, _) => weak.upgrade().is_none(),
        _ => false,
    })
}

impl UserData for DatumWrapper {
    fn add_fields<'lua, F: UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("vars", |_, this: &DatumWrapper| {
            Ok(Value::DatumIndexer(this.value))
        });
    }

    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(MetaMethod::Eq, datum_equality);
        methods.add_meta_function(MetaMethod::ToString, datum_to_string);
        methods.add_function("is_null", datum_truthiness);

        methods.add_method("get_var", |_, this, var| {
            this.value
                .upgrade()
                .ok_or_else(|| runtime!("datum was deleted"))
                .and_then(|datum| datum_get_var(&datum, var))
                .map_err(|e| external!(e.message))
        });

        methods.add_method("set_var", |_, this, (var, value)| {
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

impl UserData for DatumIndexer {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Index, |_, this, var| {
            this.value
                .upgrade()
                .ok_or_else(|| runtime!("datum was deleted"))
                .and_then(|datum| datum_get_var(&datum, var))
                .map_err(|e| external!(e.message))
        });

        methods.add_meta_method(MetaMethod::NewIndex, |_, this, (var, value)| {
            this.value
                .upgrade()
                .ok_or_else(|| runtime!("datum was deleted"))
                .and_then(|datum| datum_set_var(&datum, var, value))
                .map_err(|e| external!(e.message))
        });
    }
}

impl DatumWrapper {
    pub fn new(value: &DMValue) -> mlua::Result<Self> {
        value
            .as_weak()
            .map_err(|_| mlua::Error::ToLuaConversionError {
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

#[derive(Clone)]
pub struct GlobalIndexer {
    pub value: DMValue,
}

impl From<GlobalIndexer> for DMValue {
    fn from(wrapper: GlobalIndexer) -> Self {
        wrapper.value
    }
}

impl<T> PartialEq<T> for GlobalIndexer
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
    fn add_fields<'lua, F: UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("vars", |_, this: &GlobalWrapper| {
            Ok(Value::GlobalIndexer(this.value.clone()))
        });
    }

    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_function(MetaMethod::Eq, datum_equality);
        methods.add_meta_function(MetaMethod::ToString, datum_to_string);
        methods.add_function("is_null", datum_truthiness);

        methods.add_method("get_var", |_, this, var| {
            datum_get_var(&this.value, var).map_err(|e| external!(e.message))
        });

        methods.add_method("set_var", |_, this, (var, value)| {
            datum_set_var(&this.value, var, value).map_err(|e| external!(e.message))
        });

        methods.add_method("call_proc", |lua, this, args: MultiValue| {
            if this.value == DMValue::globals() {
                return Err(external!("Cannot call a proc on the global object"));
            }
            datum_call_proc(lua, &this.value, args)
        });
    }
}

impl UserData for GlobalIndexer {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Index, |_, this, var| {
            datum_get_var(&this.value, var).map_err(|e| external!(e.message))
        });

        methods.add_meta_method(MetaMethod::NewIndex, |_, this, (var, value)| {
            datum_set_var(&this.value, var, value).map_err(|e| external!(e.message))
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
        methods.add_meta_function(MetaMethod::Eq, datum_equality);
        methods.add_meta_function(MetaMethod::ToString, datum_to_string);
        methods.add_function("is_null", datum_truthiness);
    }
}

#[derive(PartialEq, Eq, Hash)]
enum UserDataCacheKey {
    Single(DMValue),
    Double(DMValue, DMValue),
    Triple(DMValue, DMValue, DMValue),
}

impl From<&Value> for UserDataCacheKey {
    fn from(value: &Value) -> Self {
        match value {
            Value::Datum(weak) => {
                let upgraded = weak.upgrade_or_null();
                Self::Single(upgraded)
            }
            Value::DatumIndexer(weak) => {
                let upgraded = weak.upgrade_or_null();
                Self::Double(upgraded, DMValue::null())
            }
            Value::DatumList(weak, list) => Self::Double(weak.upgrade_or_null(), list.clone()),
            Value::DatumListIndexer(weak, list) => {
                Self::Triple(weak.upgrade_or_null(), list.clone(), DMValue::null())
            }
            Value::ListRef(thing) | Value::Global(thing) | Value::Other(thing) => {
                Self::Single(thing.clone())
            }
            Value::ListIndexer(thing) | Value::GlobalIndexer(thing) => {
                Self::Double(thing.clone(), DMValue::null())
            }
            _ => unreachable!(),
        }
    }
}

fn get_cached_userdata<'lua>(lua: &'lua Lua, value: &Value) -> mlua::Result<mlua::Value<'lua>> {
    let mut userdata_pointer_cache =
        match lua.app_data_mut::<HashMap<UserDataCacheKey, *mut c_void>>() {
            Some(mut_ref) => mut_ref,
            None => {
                lua.set_app_data::<HashMap<UserDataCacheKey, *mut c_void>>(HashMap::new());
                lua.app_data_mut::<HashMap<UserDataCacheKey, *mut c_void>>()
                    .unwrap()
            }
        };
    let userdata_cache = match lua.named_registry_value("userdata_cache")? {
        mlua::Value::Table(t) => t,
        _ => {
            let new_cache = lua.create_table()?;
            lua.set_named_registry_value("userdata_cache", new_cache.clone())?;
            new_cache
        }
    };
    let key = UserDataCacheKey::from(value);
    let cached_userdata: Option<AnyUserData<'lua>> = match match userdata_pointer_cache.get(&key) {
        Some(pointer) => Some(userdata_cache.raw_get(mlua::LightUserData(*pointer))?),
        None => None,
    } {
        Some(mlua::Value::UserData(userdata)) => {
            let cached_userdata = match value {
                Value::Datum(weak) if userdata.is::<DatumWrapper>() => match (
                    weak.upgrade(),
                    userdata.borrow::<DatumWrapper>().unwrap().value.upgrade(),
                ) {
                    (Some(v1), Some(v2)) => {
                        if v1 == v2 {
                            Some(userdata.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                },
                Value::DatumIndexer(weak) if userdata.is::<DatumIndexer>() => match (
                    weak.upgrade(),
                    userdata.borrow::<DatumIndexer>().unwrap().value.upgrade(),
                ) {
                    (Some(v1), Some(v2)) => {
                        if v1 == v2 {
                            Some(userdata.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                },
                Value::DatumList(weak, list) if userdata.is::<DatumTiedList>() => {
                    let borrowed_userdata = userdata.borrow::<DatumTiedList>().unwrap();
                    match (
                        weak.upgrade(),
                        list,
                        borrowed_userdata.parent_value.upgrade(),
                        borrowed_userdata.value.clone(),
                    ) {
                        (Some(v1), l1, Some(v2), l2) => {
                            if v1 == v2 && l1 == &l2 {
                                Some(userdata.clone())
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                }
                Value::DatumListIndexer(weak, list) if userdata.is::<DatumTiedListIndexer>() => {
                    let borrowed_userdata = userdata.borrow::<DatumTiedListIndexer>().unwrap();
                    match (
                        weak.upgrade(),
                        list,
                        borrowed_userdata.parent_value.upgrade(),
                        borrowed_userdata.value.clone(),
                    ) {
                        (Some(v1), l1, Some(v2), l2) => {
                            if v1 == v2 && l1 == &l2 {
                                Some(userdata.clone())
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                }
                Value::ListRef(list) if userdata.is::<ListWrapper>() => {
                    if list == &userdata.borrow::<ListWrapper>().unwrap().value {
                        Some(userdata.clone())
                    } else {
                        None
                    }
                }
                Value::ListIndexer(list) if userdata.is::<ListIndexer>() => {
                    if list == &userdata.borrow::<ListIndexer>().unwrap().value {
                        Some(userdata.clone())
                    } else {
                        None
                    }
                }
                Value::Global(value) if userdata.is::<GlobalWrapper>() => {
                    if value == &userdata.borrow::<GlobalWrapper>().unwrap().value {
                        Some(userdata.clone())
                    } else {
                        None
                    }
                }
                Value::GlobalIndexer(value) if userdata.is::<GlobalIndexer>() => {
                    if value == &userdata.borrow::<GlobalIndexer>().unwrap().value {
                        Some(userdata.clone())
                    } else {
                        None
                    }
                }
                Value::Other(value) if userdata.is::<GenericWrapper>() => {
                    if value == &userdata.borrow::<GenericWrapper>().unwrap().value {
                        Some(userdata.clone())
                    } else {
                        None
                    }
                }
                _ => None,
            };
            mlua::Result::Ok(cached_userdata)
        }
        _ => Ok(None),
    }?;
    match cached_userdata {
        Some(userdata) => Ok(mlua::Value::UserData(userdata)),
        None => {
            let new_userdata = mlua::Value::UserData(match value {
                Value::Datum(weak) => lua.create_userdata(DatumWrapper { value: *weak }),
                Value::DatumIndexer(weak) => lua.create_userdata(DatumIndexer { value: *weak }),
                Value::DatumList(weak, list) => lua.create_userdata(DatumTiedList {
                    parent_value: *weak,
                    value: list.clone(),
                }),
                Value::DatumListIndexer(weak, list) => lua.create_userdata(DatumTiedListIndexer {
                    parent_value: *weak,
                    value: list.clone(),
                }),
                Value::ListRef(list) => lua.create_userdata(ListWrapper {
                    value: list.clone(),
                }),
                Value::ListIndexer(list) => lua.create_userdata(ListIndexer {
                    value: list.clone(),
                }),
                Value::Global(value) => lua.create_userdata(GlobalWrapper {
                    value: value.clone(),
                }),
                Value::GlobalIndexer(value) => lua.create_userdata(GlobalIndexer {
                    value: value.clone(),
                }),
                Value::Other(value) => lua.create_userdata(GenericWrapper {
                    value: value.clone(),
                }),
                _ => unreachable!(),
            }?);
            let new_userdata_pointer = new_userdata.to_pointer() as *mut c_void;
            userdata_cache.raw_set(
                mlua::LightUserData(new_userdata_pointer),
                new_userdata.clone(),
            )?;
            userdata_pointer_cache.insert(key, new_userdata_pointer);
            Ok(new_userdata)
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Null,
    Number(f32),
    String(String),
    ListRef(DMValue),
    ListIndexer(DMValue),
    List(Rc<RefCell<Vec<(Self, Self)>>>),
    DatumList(WeakValue, DMValue),
    DatumListIndexer(WeakValue, DMValue),
    Datum(WeakValue),
    DatumIndexer(WeakValue),
    Global(DMValue),
    GlobalIndexer(DMValue),
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
                Self::Global(g) => l == g,
                Self::Other(o) => l == o,
                _ => false,
            },
            Self::List(l) => match other {
                Self::List(l2) => std::ptr::eq(l.borrow().deref(), l2.borrow().deref()),
                _ => false,
            },
            Self::DatumList(w, l) => match w.upgrade() {
                Some(_) => match other {
                    Self::ListRef(l2) => l == l2,
                    Self::Global(g) => l == g,
                    Self::Other(o) => l == o,
                    _ => false,
                },
                None => matches!(other, Self::Null),
            },
            Self::Datum(w) => match w.upgrade() {
                Some(datum) => match other {
                    Self::ListRef(l) => datum == *l,
                    Self::Datum(w2) => datum == w2.upgrade_or_null(),
                    Self::Global(g) => datum == *g,
                    Self::Other(o) => datum == *o,
                    _ => false,
                },
                None => matches!(other, Self::Null),
            },
            Self::Global(g) => match other {
                Self::ListRef(l) => g == l,
                Self::Datum(w) => *g == w.upgrade_or_null(),
                Self::Global(g2) => g == g2,
                Self::Other(o) => g == o,
                _ => false,
            },
            Self::Other(o) => match other {
                Self::ListRef(l) => o == l,
                Self::Datum(w) => *o == w.upgrade_or_null(),
                Self::Global(g) => o == g,
                Self::Other(o2) => o == o2,
                _ => false,
            },
            _ => false,
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
            | ValueTag::ArgList
            | ValueTag::MobVars
            | ValueTag::ObjVars
            | ValueTag::TurfVars
            | ValueTag::AreaVars
            | ValueTag::MobContents
            | ValueTag::TurfContents
            | ValueTag::AreaContents
            | ValueTag::WorldContents
            | ValueTag::ObjContents
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
            | ValueTag::ImageOverlays
            | ValueTag::ImageUnderlays
            | ValueTag::ImageVars
            | ValueTag::TurfVisContents
            | ValueTag::ObjVisContents
            | ValueTag::MobVisContents
            | ValueTag::TurfVisLocs
            | ValueTag::ObjVisLocs
            | ValueTag::MobVisLocs
            | ValueTag::ImageVisContents
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

type ListConversionCell = RefCell<Vec<(Value, Value)>>;

impl TryFrom<Value> for DMValue {
    type Error = Runtime;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        fn list_converter(
            vec: Rc<ListConversionCell>,
            visited: &mut Vec<(Rc<ListConversionCell>, DMValue)>,
        ) -> Result<DMValue, Runtime> {
            let len = vec.borrow().iter().fold(0, |max, elem| {
                let (key, _) = elem;
                if let Value::Number(n) = key {
                    max.max(*n as u32)
                } else {
                    max
                }
            });
            let list = List::with_size(len);
            let copied_value = DMValue::from(list);
            visited.push((vec.clone(), copied_value.clone()));
            let list = copied_value.as_list().unwrap();
            for (key, value) in vec.borrow().iter() {
                let converted_key = match key {
                    Value::List(vec) => match visited.iter().find_map(|(raw, converted)| {
                        if std::ptr::eq(raw.borrow().deref(), vec.borrow().deref()) {
                            Some(converted.clone())
                        } else {
                            None
                        }
                    }) {
                        Some(converted) => converted,
                        None => list_converter(vec.clone(), visited)?,
                    },
                    anything_else => DMValue::try_from(anything_else.clone())?,
                };
                let converted_value = match value {
                    Value::List(vec) => match visited.iter().find_map(|(raw, converted)| {
                        if raw.eq(vec) {
                            Some(converted.clone())
                        } else {
                            None
                        }
                    }) {
                        Some(converted) => converted,
                        None => list_converter(vec.clone(), visited)?,
                    },
                    anything_else => DMValue::try_from(anything_else.clone())?,
                };
                list.set(converted_key, converted_value)?;
            }
            Ok(DMValue::from(list))
        }

        match value {
            Value::Null => Ok(DMValue::null()),
            Value::Number(n) => Ok(DMValue::from(n)),
            Value::String(s) => DMValue::from_string(s),
            Value::ListRef(l) | Value::ListIndexer(l) => Ok(l),
            Value::DatumList(w, l) | Value::DatumListIndexer(w, l) => match w.upgrade() {
                Some(_) => Ok(l),
                None => Ok(DMValue::null()),
            },
            Value::List(vec) => list_converter(vec, &mut Vec::new()),
            Value::Datum(weak) | Value::DatumIndexer(weak) => Ok(weak.upgrade_or_null()),
            Value::Global(glob) | Value::GlobalIndexer(glob) => Ok(glob),
            Value::Other(other) => Ok(other),
        }
    }
}

impl<'lua> ToLua<'lua> for Value {
    fn to_lua(self, lua: &mlua::Lua) -> mlua::Result<MluaValue> {
        fn list_converter<'lua>(
            vec: Rc<ListConversionCell>,
            visited: &mut Vec<(Rc<ListConversionCell>, Table<'lua>)>,
            lua: &'lua Lua,
        ) -> mlua::Result<Table<'lua>> {
            let table = lua.create_table()?;
            visited.push((vec.clone(), table.clone()));
            for (key, value) in vec.borrow().iter() {
                let converted_key = match key {
                    Value::List(vec) => match visited.iter().find_map(|(raw, converted)| {
                        if std::ptr::eq(raw.borrow().deref(), vec.borrow().deref()) {
                            Some(converted.clone())
                        } else {
                            None
                        }
                    }) {
                        Some(converted) => MluaValue::Table(converted),
                        None => MluaValue::Table(list_converter(vec.clone(), visited, lua)?),
                    },
                    anything_else => anything_else.clone().to_lua(lua)?,
                };
                let converted_value = match value {
                    Value::List(vec) => match visited.iter().find_map(|(raw, converted)| {
                        if raw.eq(vec) {
                            Some(converted.clone())
                        } else {
                            None
                        }
                    }) {
                        Some(converted) => MluaValue::Table(converted),
                        None => MluaValue::Table(list_converter(vec.clone(), visited, lua)?),
                    },
                    anything_else => anything_else.clone().to_lua(lua)?,
                };
                table.raw_set(converted_key, converted_value)?;
            }
            Ok(table)
        }
        match self {
            Self::Null => Ok(mlua::Nil),
            Self::Number(n) => Ok(MluaValue::Number(n as f64)),
            Self::String(s) => Ok(MluaValue::String(lua.create_string(&s)?)),
            Self::ListRef(l) | Self::ListIndexer(l) if matches!(l.raw.tag,
                ValueTag::MobVars
                | ValueTag::ObjVars
                | ValueTag::TurfVars
                | ValueTag::AreaVars
                | ValueTag::ClientVars
                | ValueTag::ImageVars
                | ValueTag::Vars) => Err(ToLuaConversionError{
                from: "datum vars",
                to: "userdata",
                message: Some(String::from("Cannot guarantee the validity of vars lists without a weak reference to the datum they are a variable of. Use datum:get_var(\"vars\") instead."))}),
            Self::ListRef(l) | Self::ListIndexer(l) if matches!(l.raw.tag,
                    ValueTag::MobContents
                    | ValueTag::TurfContents
                    | ValueTag::AreaContents
                    | ValueTag::ObjContents) => Err(ToLuaConversionError{
                        from: "atom contents",
                        to: "userdata",
                        message: Some(String::from("Cannot guarantee the validity of atom contents lists without a weak reference to the atom they are a variable of. Use datum:get_var(\"contents\") instead."))
                    }),
            Self::ListRef(l) | Self::ListIndexer(l) if matches!(l.raw.tag,
                    ValueTag::MobOverlays
                    | ValueTag::ObjOverlays
                    | ValueTag::TurfOverlays
                    | ValueTag::AreaOverlays
                    | ValueTag::ImageOverlays) => Err(ToLuaConversionError{
                        from: "atom overlays",
                        to: "userdata",
                        message: Some(String::from("Cannot guarantee the validity of atom overlays lists without a weak reference to the atom they are attached to. Use datum:get_var(\"overlays\") instead."))
                    }),
            Self::ListRef(l) | Self::ListIndexer(l) if matches!(l.raw.tag,
                    ValueTag::ObjUnderlays
                    | ValueTag::MobUnderlays
                    | ValueTag::TurfUnderlays
                    | ValueTag::AreaUnderlays
                    | ValueTag::ImageUnderlays) => Err(ToLuaConversionError{
                        from: "atom underlays",
                        to: "userdata",
                        message: Some(String::from("Cannot guarantee the validity of atom underlays lists without a weak reference to the atom they are attached to. Use datum:get_var(\"underlays\") instead."))
                    }),
            Self::ListRef(l) | Self::ListIndexer(l) if matches!(l.raw.tag,
                    ValueTag::TurfVisContents
                    | ValueTag::ObjVisContents
                    | ValueTag::MobVisContents
                    | ValueTag::ImageVisContents) => Err(ToLuaConversionError{
                        from: "atom vis_contents",
                        to: "userdata",
                        message: Some(String::from("Cannot guarantee the validity of atom vis_contents lists without a weak reference to the atom they are attached to. Use datum:get_var(\"vis_contents\") instead."))
                    }),
            Self::ListRef(l) | Self::ListIndexer(l) if matches!(l.raw.tag,
                    ValueTag::TurfVisLocs
                    | ValueTag::ObjVisLocs
                    | ValueTag::MobVisLocs) => Err(ToLuaConversionError{
                        from: "atom vis_locs",
                        to: "userdata",
                        message: Some(String::from("Cannot guarantee the validity of atom vis_locs lists without a weak reference to the atom they are attached to. Use datum:get_var(\"vis_locs\") instead."))
                    }),
            Self::List(vec) => Ok(MluaValue::Table(list_converter(vec, &mut vec![], lua)?)),
            Self::ListRef(_)
            | Self::ListIndexer(_)
            | Self::DatumList(_, _)
            | Self::DatumListIndexer(_, _)
            | Self::Datum(_)
            | Self::DatumIndexer(_)
            | Self::Global(_)
            | Self::GlobalIndexer(_)
            | Self::Other(_) => {
                Ok(get_cached_userdata(lua, &self)?)
            }
        }
    }
}

impl<'lua> FromLua<'lua> for Value {
    fn from_lua(value: MluaValue, lua: &mlua::Lua) -> mlua::Result<Self> {
        fn table_conversion<'lua>(
            table: Table<'lua>,
            visited: &mut Vec<(*const c_void, Rc<ListConversionCell>)>,
            lua: &Lua,
        ) -> mlua::Result<Value> {
            let list: Rc<ListConversionCell> = Rc::new(RefCell::new(vec![]));
            visited.push((table.to_pointer(), list.clone()));
            for pair in table.pairs() {
                let (key, value): (MluaValue, MluaValue) = pair?;
                let converted_key = match key {
                    MluaValue::Table(t) => {
                        match visited.iter().find_map(|(raw, converted)| {
                            if *raw == t.to_pointer() {
                                Some(converted)
                            } else {
                                None
                            }
                        }) {
                            Some(converted) => Value::List(converted.clone()),
                            None => table_conversion(t, visited, lua)?,
                        }
                    }
                    anything_else => Value::from_lua(anything_else, lua)?,
                };
                let converted_value = match value {
                    MluaValue::Table(t) => {
                        match visited.iter().find_map(|(raw, converted)| {
                            if *raw == t.to_pointer() {
                                Some(converted)
                            } else {
                                None
                            }
                        }) {
                            Some(converted) => Value::List(converted.clone()),
                            None => table_conversion(t, visited, lua)?,
                        }
                    }
                    anything_else => Value::from_lua(anything_else, lua)?,
                };
                list.borrow_mut().push((converted_key, converted_value));
            }
            Ok(Value::List(list))
        }
        let typename = value.type_name();
        let pointer = value.to_pointer();
        match value {
            MluaValue::Nil => Ok(Self::Null),
            MluaValue::Boolean(b) => Ok(Self::Number(if b { 1.0 } else { 0.0 })),
            MluaValue::Integer(i) => Ok(Self::Number(i as f32)),
            MluaValue::Number(n) => Ok(Self::Number(n as f32)),
            MluaValue::String(s) => Ok(Self::String(String::from(s.to_str()?))),
            MluaValue::Table(t) => table_conversion(t, &mut vec![], lua),
            MluaValue::UserData(ud) => {
                if let Ok(list) = ud.borrow::<ListWrapper>() {
                    Ok(Self::ListRef(list.value.clone()))
                } else if let Ok(indexer) = ud.borrow::<ListIndexer>() {
                    Ok(Self::ListIndexer(indexer.value.clone()))
                } else if let Ok(tied_list) = ud.borrow::<DatumTiedList>() {
                    Ok(Self::DatumList(
                        tied_list.parent_value,
                        tied_list.value.clone(),
                    ))
                } else if let Ok(indexer) = ud.borrow::<DatumTiedListIndexer>() {
                    Ok(Self::DatumListIndexer(
                        indexer.parent_value,
                        indexer.value.clone(),
                    ))
                } else if let Ok(datum) = ud.borrow::<DatumWrapper>() {
                    Ok(Self::Datum(datum.value))
                } else if let Ok(indexer) = ud.borrow::<DatumIndexer>() {
                    Ok(Self::DatumIndexer(indexer.value))
                } else if let Ok(global) = ud.borrow::<GlobalWrapper>() {
                    Ok(Self::Global(global.value.clone()))
                } else if let Ok(indexer) = ud.borrow::<GlobalIndexer>() {
                    Ok(Self::Global(indexer.value.clone()))
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
