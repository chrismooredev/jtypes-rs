
use std::borrow::{Borrow, Cow};
use std::fmt;

use cesu8str::{Cesu8Str, Variant};

use crate::names::{BinaryClassname, InternalClassname, ClassnameParsingError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JavaType {
	/// No type (`V`)
	Void,
	/// boolean (`Z`)
	Boolean,
	/// Signed 8-bit integer (`B`)
	Byte,
	/// Unsigned 16-bit integer (`C`)
	Char,
	/// Signed 16-bit integer (`S`)
	Short,
	/// Signed 32-bit integer (`I`)
	Int,
	/// Signed 64-bit integer (`J`)
	Long,
	/// 32-bit Floating-point number (`F`)
	Float,
	/// 64-bit Floating-point number (`D`)
	Double,
	/// Java Object (`L<class>;`)
	Object(InternalClassname),
	/// Array of types (`[type`)
	///
	/// A dimensionality of more than one should be represented by nested JavaTypes:
	/// ex: Java type `boolean[][][]` would be:
	/// ```
	/// # use jtypes::JavaType;
	///
	/// let bool_3d = JavaType::Array(Box::new(
	///     JavaType::Array(Box::new(
	///         JavaType::Array(Box::new(
	///             JavaType::Boolean
	///         ))
	///     ))
	/// ));
	/// 
	/// assert_eq!(bool_3d.encode(), "[[[Z")
	/// ```
	///
	/// The same representation can be more easily created with `JavaType::nested_array(JavaType::Boolean, 3)`
	///
	Array(Box<JavaType>),
}
impl fmt::Display for JavaType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			JavaType::Void => write!(f, "void"),
			JavaType::Boolean => write!(f, "boolean"),
			JavaType::Byte => write!(f, "byte"),
			JavaType::Char => write!(f, "char"),
			JavaType::Short => write!(f, "short"),
			JavaType::Int => write!(f, "int"),
			JavaType::Long => write!(f, "long"),
			JavaType::Float => write!(f, "float"),
			JavaType::Double => write!(f, "double"),
			JavaType::Object(ic) => write!(f, "{}", BinaryClassname::from(ic)),
			JavaType::Array(inner) => write!(f, "{}[]", inner),
		}
	}
}
impl JavaType {
	pub fn object<I: Into<InternalClassname>>(cls: I) -> JavaType {
		JavaType::Object(cls.into())
	}
	pub fn try_object<S: Into<Cow<'static, str>>>(cls: S) -> Result<JavaType, ClassnameParsingError> {
		InternalClassname::new(cls).map(JavaType::Object)
	}

	pub fn array<I: Into<JavaType>>(cls: I) -> JavaType {
		JavaType::Array(Box::new(cls.into()))
	}

	/// Creates a nested array of the specified depth. If the depth is zero, then the type is returned as is.
	pub fn nested_array<I: Into<JavaType>>(cls: I, depth: u8) -> JavaType {
		let mut rtn = cls.into();
		for _ in 0..depth {
			rtn = JavaType::array(rtn);
		}
		rtn
	}

	pub fn encode(&self) -> Cow<'static, str> {
		use JavaType::*;
		match self {
			Void => Cow::Borrowed("V"),
			Boolean => Cow::Borrowed("Z"),
			Byte => Cow::Borrowed("B"),
			Char => Cow::Borrowed("C"),
			Short => Cow::Borrowed("S"),
			Int => Cow::Borrowed("I"),
			Long => Cow::Borrowed("J"),
			Float => Cow::Borrowed("F"),
			Double => Cow::Borrowed("D"),
			Object(ic) => Cow::Owned(format!("L{};", ic)),
			Array(inner) => Cow::Owned(format!("[{}", inner.encode())),
		}
	}

	/// Parses one JavaType from the beginning of the string, returning how many bytes it consumed.
	pub fn parse_one(s: &str) -> Result<(JavaType, usize), JavaTypeParseError> {		
		let mut iter = s.char_indices();
		let first = iter.next();
		Ok(match first.map(|(_, c)| c) {
			Some('V') => (JavaType::Void, 1),
			Some('Z') => (JavaType::Boolean, 1),
			Some('B') => (JavaType::Byte, 1),
			Some('C') => (JavaType::Char, 1),
			Some('S') => (JavaType::Short, 1),
			Some('I') => (JavaType::Int, 1),
			Some('J') => (JavaType::Long, 1),
			Some('F') => (JavaType::Float, 1),
			Some('D') => (JavaType::Double, 1),
			Some('L') => {
				let classname: String = { (&mut iter).map(|(_, c)| c).take_while(|c| *c != ';').collect::<String>() };
				
				let consumed = match iter.next() {
					// `consumed` provides the next char index after the ending semicolon;
					// accounting for zero-index, this should be the consumed bytes
					Some((consumed, _)) => consumed,

					// no more chars left - we've consumed the whole string
					None => s.len(),
				};

				(JavaType::Object(InternalClassname::new(classname)?), consumed)
			},
			Some('[') => {
				let (jt, consumed) = JavaType::parse_one(&s[1..])?;
				(JavaType::array(jt), 1 + consumed)
			},
			_ => return Err(JavaTypeParseError::UnknownType(s.to_owned())),
		})
	}
}


#[derive(Debug, thiserror::Error)]
pub enum JavaTypeParseError {
	#[error("invalid java type string: {:?}", .0)]
	UnknownType(String),

	#[error("unable to validate object classname")]
	BadClassname(#[from] ClassnameParsingError),

	#[error("extra characters found after parsing one JavaType (whole string: {:?})", .0)]
	ExtraCharacters(String),
}
impl std::str::FromStr for JavaType {
	type Err = JavaTypeParseError;
	fn from_str(s: &str) -> Result<JavaType, Self::Err> {
		let (jt, consumed) = JavaType::parse_one(s)?;
		if consumed != s.len() {
			return Err(JavaTypeParseError::ExtraCharacters(s.to_owned()));
		}

		Ok(jt)
	}
}
impl From<InternalClassname> for JavaType {
	fn from(ic: InternalClassname) -> JavaType {
		JavaType::Object(ic)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodSignature {
	args: Vec<JavaType>,
	rtn: JavaType,
}
impl MethodSignature {
	pub fn serialize(&self) -> MethodSignatureDesc<'static> {
		// we have created it, so we know it is valid - create it directly
		MethodSignatureDesc(Cow::Owned(
			format!("({}){}", self.args.iter().map(|jt| jt.encode()).collect::<String>(), self.rtn.encode())
		))
	}

	pub fn parameters(&self) -> &[JavaType] {
		&self.args
	}
	pub fn returns(&self) -> &JavaType {
		&self.rtn
	}
}
impl<'a> From<&MethodSignatureDesc<'a>> for MethodSignature {
	fn from(msd: &MethodSignatureDesc<'a>) -> MethodSignature {
		msd.as_str().parse().expect("already validated method signature should parse without error")
	}
}
impl fmt::Display for MethodSignature {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Display::fmt(&self.serialize(), f)
	}
}


#[derive(Debug, thiserror::Error)]
pub enum MethodSignatureParseError {
	#[error("method signature is not in `(<optional args>)<rtn>` form (sig: {:?})", .0)]
	BadForm(String),

	#[error("malformed argument list")]
	BadArgumentList(#[source] JavaTypeParseError),

	#[error("unable to parse method signature return type")]
	BadReturnType(#[source] JavaTypeParseError),
}
impl std::str::FromStr for MethodSignature {
	type Err = MethodSignatureParseError;
	fn from_str(s: &str) -> Result<MethodSignature, Self::Err> {
		if ! s.starts_with('(') {
			return Err(MethodSignatureParseError::BadForm(s.to_owned()));
		}

		let mut rest = &s[1..];
		let mut args = Vec::new();
		while ! rest.starts_with(')') && ! rest.is_empty() {
			let (next_arg, len) = JavaType::parse_one(rest)
				.map_err(MethodSignatureParseError::BadArgumentList)?;
			rest = &rest[len..];
			args.push(next_arg);
		}

		match rest.chars().next() {
			None => return Err(MethodSignatureParseError::BadForm(s.to_owned())),
			Some(')') => { /* expected, continue */ },
			Some(c) => panic!("found unexpected character in arg list (expected ')', got {:?})", c),
		}

		let rtn: JavaType = rest[1..].parse()
			.map_err(MethodSignatureParseError::BadReturnType)?;

		Ok(MethodSignature {
			args,
			rtn,
		})
	}
}

/// A string-based, validated Java method signature.
#[derive(Debug)]
pub struct MethodSignatureDesc<'a>(Cow<'a, str>);
impl<'a> MethodSignatureDesc<'a> {
	pub fn new(s: &'a str) -> Result<Self, MethodSignatureParseError> {
		let _: MethodSignature = s.parse()?;
		// if the string is malformed, then it would have returned a bubble-up'd error
		Ok(MethodSignatureDesc(Cow::Borrowed(s)))
	}
	pub unsafe fn new_unchecked(s: Cow<'a, str>) -> MethodSignatureDesc {
		MethodSignatureDesc(s)
	}
	pub fn into_inner(self) -> Cow<'a, str> {
		self.0
	}
	pub fn as_str(&self) -> &str {
		self.0.borrow()
	}
	pub fn into_cesu8(self) -> Cesu8Str<'a> {
		Cesu8Str::from_utf8(self.0, Variant::Java)
	}
}
impl<'a> AsRef<str> for MethodSignatureDesc<'a> {
	fn as_ref(&self) -> &str {
		self.0.borrow()
	}
}
impl<'a> Borrow<str> for MethodSignatureDesc<'a> {
	fn borrow(&self) -> &str {
		self.0.borrow()
	}
}
impl From<MethodSignature> for MethodSignatureDesc<'static> {
	fn from(ms: MethodSignature) -> MethodSignatureDesc<'static> {
		ms.serialize()
	}
}
impl<'a> fmt::Display for MethodSignatureDesc<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Display::fmt(&self.0, f)
	}
}

#[test]
fn signature_to_str() {
	let sig = MethodSignature {
		args: vec![
			JavaType::Int,
			JavaType::object(InternalClassname::new("java/lang/String").unwrap()),
			JavaType::array(JavaType::Int),
		],
		rtn: JavaType::Long,
	};

	assert_eq!(sig.serialize().as_str(), "(ILjava/lang/String;[I)J");
}