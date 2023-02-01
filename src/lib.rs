
pub use jtypes_core::{names, signature};
pub use jtypes_macros::*;

// allow using ::jtypes within jtypes_macros invocations within this crate. meta.
extern crate self as jtypes;

pub use names::{BinaryClassname, Components, GenericComponents, InternalClassname};
pub use signature::{JavaType, MethodSignature};

::lazy_static::lazy_static! {
	pub static ref JAVA_LANG_OBJECT: InternalClassname = cls!("java/lang/Object");
	pub static ref JAVA_LANG_CLASS: InternalClassname = cls!("java/lang/Class");
	pub static ref JAVA_LANG_STRING: InternalClassname = cls!("java/lang/String");
	pub static ref JAVA_LANG_SYSTEM: InternalClassname = cls!("java/lang/System");
	pub static ref JAVA_LANG_THROWABLE: InternalClassname = cls!("java/lang/Throwable");
}
