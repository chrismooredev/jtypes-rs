
use std::borrow::Cow;
use std::fmt;

#[derive(thiserror::Error, Debug)]
pub enum ClassnameParsingError {
	#[error("An invalid character {:?} was found within an identifier segment {:?} (wrong classname type?)", .1, .0)]
	BadCharacter(String, char),
	#[error("Failed to create an internal format classname (ex \"java/lang/String\") from {:?}", .0)]
	NotInternalFormat(String),
	#[error("Failed to create a binary format classname (ex \"java.lang.String\") from {:?}", .0)]
	NotBinaryFormat(String),
	
	#[error("Attempted to parse a classname from an empty string")]
	EmptyString,

	#[error("Attempted to parse a classname with empty components")]
	EmptyComponent(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternalClassname(Cow<'static, str>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinaryClassname(Cow<'static, str>);

#[derive(Debug, Clone, Hash)]
pub struct GenericComponents<S> {
	/// Package + terminal class name
	base_class: Vec<S>,

	/// Arbitrary number of nested inner classes
	inner_classes: Vec<S>,
}
pub type Components = GenericComponents<String>;
pub type ComponentsRef<'a> = GenericComponents<&'a str>;

impl InternalClassname {
	pub const JAVA_LANG_OBJECT: InternalClassname = unsafe { InternalClassname::new_unchecked_const("java/lang/Object") };
	pub const JAVA_LANG_CLASS: InternalClassname = unsafe { InternalClassname::new_unchecked_const("java/lang/Class") };
	pub const JAVA_LANG_STRING: InternalClassname = unsafe { InternalClassname::new_unchecked_const("java/lang/String") };
	pub const JAVA_LANG_SYSTEM: InternalClassname = unsafe { InternalClassname::new_unchecked_const("java/lang/System") };

	/// Creates a new class name using internal format (ex: `java/lang/Object`). Returns an error if it is invalid or empty.
	pub fn new<IC: Into<Cow<'static, str>>>(cls: IC) -> Result<InternalClassname, ClassnameParsingError> {
		let cls: Cow<'static, str> = cls.into();

		verify_classname_str(&cls, '/')?;

		Ok(InternalClassname(cls))
	}

	/// Creates a new class name using internal format (ex: `java/lang/Object`).
	/// 
	/// # Safety
	/// The internal name must be valid, otherwise unexpected behavior may occur in other functions.
	/// 
	/// Validation checking can be done with `InternalClassname::new(...)`
	pub unsafe fn new_unchecked<IC: Into<String>>(cls: IC) -> InternalClassname {
		InternalClassname(Cow::Owned(cls.into()))
	}

	/// Creates a new class name using internal format (ex: `java/lang/Object`).
	/// 
	/// # Safety
	/// The internal name must be valid, otherwise unexpected behavior may occur in other functions.
	/// 
	/// Validation checking can be done with `InternalClassname::new(...)`
	pub const unsafe fn new_unchecked_const(cls: &'static str) -> InternalClassname {
		InternalClassname(Cow::Borrowed(cls))
	}

	pub fn as_str(&self) -> &str {
		&*self.0
	}

	pub fn to_binary(&self) -> BinaryClassname {
		BinaryClassname::from(self)
	}
	pub fn to_components(&self) -> Components {
		Components::from(self)
	}
	pub fn as_components(&self) -> ComponentsRef<'_> {
		ComponentsRef::from(self)
	}
}
impl BinaryClassname {
	
	/// Creates a new class name using binary format (ex: `java.lang.Object`). Returns an error if it is invalid or empty.
	pub fn new<IC: Into<Cow<'static, str>>>(cls: IC) -> Result<BinaryClassname, ClassnameParsingError> {
		let cls: Cow<'static, str> = cls.into();
		
		verify_classname_str(&cls, '.')?;
		
		Ok(BinaryClassname(cls))
	}

	
	/// Creates a new class name using binary format (ex: `java.lang.Object`).
	/// 
	/// # Safety
	/// The binary class name must be valid, otherwise unexpected behavior may occur in other functions.
	/// 
	/// Validation checking can be done with `BinaryClassname::new(...)`
	pub const unsafe fn new_unchecked_const(cls: &'static str) -> BinaryClassname {
		BinaryClassname(Cow::Borrowed(cls))
	}

	pub fn to_internal(&self) -> InternalClassname {
		InternalClassname::from(self)
	}
	pub fn to_components(&self) -> Components {
		Components::from(self)
	}
	pub fn as_components(&self) -> ComponentsRef<'_> {
		ComponentsRef::from(self)
	}
}

impl<S: AsRef<str>> GenericComponents<S> {
	pub fn new(class: Vec<S>, inner_class: Vec<S>) -> Result<Self, ClassnameParsingError> {
		// TODO: verify GenericComponents::new
		
		if class.is_empty() {
			/* ignore inner_class, there MUST be a regular `class` part, even if inner_class parts exist */
			return Err(ClassnameParsingError::EmptyString);
		}

		let get_full_name = || {
			let mut clsiter = class.iter();
			let mut joined: String = clsiter.next().unwrap().as_ref().to_owned();
			
			for seg in clsiter {
				joined.push('.');
				joined.push_str(seg.as_ref());
			}
			for seg in &inner_class {
				joined.push('$');
				joined.push_str(seg.as_ref());
			}
			joined
		};
		
		for seg in &class {
			verify_component(seg.as_ref(), get_full_name)?;
		}
		for seg in &inner_class {
			verify_component(seg.as_ref(), get_full_name)?;
		}

		Ok(Self {
			base_class: class,
			inner_classes: inner_class
		})
	}

	/// Creates a new `Components` struct.
	///
	/// `class` contains the separated package and classname of the class.
	/// `inner_class` is used to specify the specific inner class of `class`. If empty, then this represents the base class.
	/// 
	/// # Safety
	/// Does not check class segments for validity. Other functions may error
	pub unsafe fn new_unchecked(class: Vec<S>, inner_class: Vec<S>) -> Self {
		Self {
			base_class: class,
			inner_classes: inner_class,
		}
	}

	/// Returns a slice containing references to the containing class' package and classname.
	///
	/// The last element will always be the actual class' name
	///
	/// Note that this ignores any possible inner class descriptions
	///
	/// Ex:
	/// ```
	/// # use jtypes::{InternalClassname, Components};
	///
	/// let class = InternalClassname::new("java/util/HashMap$Node").unwrap();
	/// let components = Components::from(&class);
	/// assert_eq!(components.packages_class(), &["java", "util", "HashMap"]);
	/// ```
	pub fn packages_class(&self) -> &[S] {
		&self.base_class
	}
	
	/// Returns a slice containing references to the containing class' inner classes, if any.
	///
	/// The last element will always be the deepest inner class' name
	///
	/// Note that this ignores the package and actual classname
	///
	/// Ex:
	/// ```
	/// # use jtypes::{InternalClassname, Components};
	///
	/// let class = InternalClassname::new("java/util/HashMap$Node").unwrap();
	/// let components = Components::from(&class);
	/// assert_eq!(components.inner_classes(), &["Node"]);
	/// ```
	pub fn inner_classes(&self) -> &[S] {
		&self.inner_classes
	}
}
impl<'a, S: From<&'a str>> GenericComponents<S> {
	/// Used internally to convert internal and binary class descriptors into individual components
	fn split_from_string(string: &'a str, sep: char) -> GenericComponents<S> {
		// Note: Inner classes have a dollar sign prefix, specified after the base class
		// As it uses the same separater, it does not have to be specially treated here

		// TODO: see how this interacts with arrays/etc. Is that even necessary here?

		let mut inner_iter = string.split('$');
		let base_class = inner_iter.next().expect("there should be at least one base class");
		let base_class: Vec<_> = base_class.split(sep).map(|s| s.into()).collect();
		let inner_class = inner_iter.map(|s| s.into()).collect();
		assert!(!base_class.is_empty());
		
		GenericComponents {
			base_class,
			inner_classes: inner_class,
		}
	}
}
impl<S: AsRef<str>> GenericComponents<S> {
	pub fn as_str(&self) -> GenericComponents<&'_ str> {
		GenericComponents {
			base_class: self.base_class.iter().map(|s| s.as_ref()).collect(),
			inner_classes: self.inner_classes.iter().map(|s| s.as_ref()).collect(),
		}
	}
	pub fn to_internal(&self) -> InternalClassname {
		InternalClassname::from(self)
	}
	pub fn to_binary(&self) -> BinaryClassname {
		BinaryClassname::from(self)
	}

	// Needs #![feature(slice_concat_trait)] - a nightly dependency
	// impl<S> GenericComponents<S> 
	// where
	//     S: AsRef<str>,
	//     [S]: std::slice::Join<S, Output = String>,
	// {
	// 	fn combine_to_string(&self, sep: &'_ str) -> String {
	// 		let mut base: String = self.base_class.join(sep);
	// 		for ic in &self.inner_class {
	// 			base += "$";
	// 			base += ic;
	// 		}
	// 		base
	// 	}
	// }
	fn combine_to_string(&self, sep: &'_ str) -> String {
		let mut base = String::new();

		for (i, bc) in self.base_class.iter().enumerate() {
			base += bc.as_ref();
			if i != self.base_class.len() - 1 {
				base += sep;
			}
		}

		for ic in &self.inner_classes {
			base += "$";
			base += ic.as_ref();
		}

		base
	}
}

#[test]
fn test_from_lifetimes() {
	let intclsname = InternalClassname::new("java/util/HashMap$Node").unwrap();
	let _gcstr: GenericComponents<&str> = (&intclsname).into();
}

// Convert between types

// InternalClassname <-> BinaryClassname
impl From<&InternalClassname> for BinaryClassname {
	fn from(int: &InternalClassname) -> BinaryClassname {
		// Note: Inner classes have a dollar sign prefix, specified after the base class
		// As it uses the same seperator, it does not have to be specially treated here

		// TODO: see how this interacts with arrays/etc. Is that even necessary here?
		// TODO: provide implementation that changes underlying str? (eg no alloc) would need unsafe, likely not worth the effort for any usecases
		
		BinaryClassname(int.0.split('/').collect::<Vec<_>>().join(".").into())
	}
}
impl From<&BinaryClassname> for InternalClassname {
	fn from(bin: &BinaryClassname) -> InternalClassname {
		// Note: Inner classes have a dollar sign prefix, specified after the base class
		// As it uses the same separater, it does not have to be specially treated here

		// TODO: see how this interacts with arrays/etc. Is that even necessary here?
		InternalClassname(bin.0.split('.').collect::<Vec<_>>().join("/").into())
	}
}
impl From<InternalClassname> for BinaryClassname { fn from(item: InternalClassname) -> Self { (&item).into() } }
impl From<BinaryClassname> for InternalClassname { fn from(item: BinaryClassname) -> Self { (&item).into() } }

// InternalClassname <-> Components
impl<'a, S: From<&'a str>> From<&'a InternalClassname> for GenericComponents<S> {
	fn from(int: &'a InternalClassname) -> Self {
		Self::split_from_string(int.0.as_ref(), '/')
	}
}
impl<S: AsRef<str>> From<&GenericComponents<S>> for InternalClassname {
	fn from(comp: &GenericComponents<S>) -> InternalClassname {
		InternalClassname(comp.combine_to_string("/").into())
	}
}

impl From<InternalClassname> for GenericComponents<String> { fn from(item: InternalClassname) -> Self { item.into() } }
impl<S: AsRef<str>> From<GenericComponents<S>> for InternalClassname { fn from(item: GenericComponents<S>) -> Self { (&item).into() } }

// BinaryClassname <-> Components
impl<'a, S: From<&'a str>> From<&'a BinaryClassname> for GenericComponents<S> {
	fn from(int: &'a BinaryClassname) -> Self {
		Self::split_from_string(&int.0, '.')
	}
}
impl<S: AsRef<str>> From<&GenericComponents<S>> for BinaryClassname {
	fn from(comp: &GenericComponents<S>) -> Self {
		BinaryClassname(comp.combine_to_string(".").into())
	}
}
impl From<BinaryClassname> for GenericComponents<String> { fn from(item: BinaryClassname) -> Self { item.into() } }
impl<S: AsRef<str>> From<GenericComponents<S>> for BinaryClassname { fn from(item: GenericComponents<S>) -> Self { (&item).into() } }

impl<'a, S: AsRef<str>> From<&'a GenericComponents<S>> for GenericComponents<&'a str> { fn from(item: &'a GenericComponents<S>) -> Self { item.as_str() } }

// Create PartialEq implementations between types (all defined in terms of Components<'a> == Components<'a>
macro_rules! impl_classname_eq_no_components {
	($first: ty, $second: ty) => {
		#[allow(clippy::cmp_owned)]
		impl PartialEq<$second> for $first {
			fn eq(&self, other: &$second) -> bool {
				GenericComponents::<&str>::from(self) == GenericComponents::<&str>::from(other)
			}
		}
		#[allow(clippy::cmp_owned)]
		impl PartialEq<$first> for $second {
			fn eq(&self, other: &$first) -> bool {
				GenericComponents::<&str>::from(self) == GenericComponents::<&str>::from(other)
			}
		}
		#[allow(clippy::cmp_owned)]
		impl PartialOrd<$second> for $first {
			fn partial_cmp(&self, other: &$second) -> Option<std::cmp::Ordering> {
				GenericComponents::<&str>::from(self).partial_cmp(&GenericComponents::<&str>::from(other))
			}
		}
		#[allow(clippy::cmp_owned)]
		impl PartialOrd<$first> for $second {
			fn partial_cmp(&self, other: &$first) -> Option<std::cmp::Ordering> {
				GenericComponents::<&str>::from(self).partial_cmp(&GenericComponents::<&str>::from(other))
			}
		}
	}
}

macro_rules! impl_classname_eq_one_components {
	($first: ty) => {
		#[allow(clippy::cmp_owned)]
		impl<S: AsRef<str>> PartialEq<GenericComponents<S>> for $first {
			fn eq(&self, other: &GenericComponents<S>) -> bool {
				GenericComponents::<&str>::from(self) == other.as_str()
			}
		}
		#[allow(clippy::cmp_owned)]
		impl<S: AsRef<str>> PartialEq<$first> for GenericComponents<S> {
			fn eq(&self, other: &$first) -> bool {
				GenericComponents::<&str>::from(other) == self.as_str()
			}
		}
		
		#[allow(clippy::cmp_owned)]
		impl<S: AsRef<str>> PartialOrd<GenericComponents<S>> for $first {
			fn partial_cmp(&self, other: &GenericComponents<S>) -> Option<std::cmp::Ordering> {
				GenericComponents::<&str>::from(self).partial_cmp(&other.as_str())
			}
		}
		#[allow(clippy::cmp_owned)]
		impl<S: AsRef<str>> PartialOrd<$first> for GenericComponents<S> {
			fn partial_cmp(&self, other: &$first) -> Option<std::cmp::Ordering> {
				self.as_str().partial_cmp(&GenericComponents::<&str>::from(other))
			}
		}
	}
}

impl<S1: AsRef<str>, S2: AsRef<str>> PartialEq<GenericComponents<S2>> for GenericComponents<S1> {
	fn eq(&self, other: &GenericComponents<S2>) -> bool {
		self.as_str() == other.as_str()
	}
}
impl<S: AsRef<str>> Eq for GenericComponents<S> {}

impl<S1: AsRef<str>, S2: AsRef<str>> PartialOrd<GenericComponents<S2>> for GenericComponents<S1> {
	fn partial_cmp(&self, other: &GenericComponents<S2>) -> Option<std::cmp::Ordering> {
		let this = self.as_str();
		let other = other.as_str();

		let bc = this.base_class.partial_cmp(&other.base_class).expect("there should always be a compare result for two Vec<&str>'s");
		if bc != std::cmp::Ordering::Equal {
			return Some(bc);
		}

		Some(this.inner_classes.partial_cmp(&other.inner_classes).expect("there should always be a compare result for two Vec<&str>'s"))
	}
}
impl<S: AsRef<str>> Ord for GenericComponents<S> {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.partial_cmp(other).unwrap() }
}


// PartialOrd, Ord, 

// InternalClassname <-> BinaryClassname
impl_classname_eq_no_components!(InternalClassname, BinaryClassname);
// InternalClassname <-> Components
impl_classname_eq_one_components!(InternalClassname);
// BinaryClassname <-> Components
impl_classname_eq_one_components!(BinaryClassname);

// Deref for InternalClassname/BinaryClassname
impl AsRef<str> for InternalClassname {
	fn as_ref(&self) -> &str {
		&*self.0
	}
}
impl AsRef<str> for BinaryClassname {
	fn as_ref(&self) -> &str {
		&*self.0
	}
}

impl fmt::Display for InternalClassname {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Display::fmt(&self.0, f)
	}
}
impl fmt::Display for BinaryClassname {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Display::fmt(&self.0, f)
	}
}

impl std::convert::From<&InternalClassname> for InternalClassname {
	fn from(ic: &InternalClassname) -> InternalClassname {
		ic.clone()
	}
}
impl std::convert::From<&BinaryClassname> for BinaryClassname {
	fn from(ic: &BinaryClassname) -> BinaryClassname {
		ic.clone()
	}
}

impl From<InternalClassname> for String {
	fn from(ic: InternalClassname) -> String {
		Cow::into_owned(ic.0)
	}
}
impl From<BinaryClassname> for String {
	fn from(ic: BinaryClassname) -> String {
		Cow::into_owned(ic.0)
	}
}

fn verify_classname_str(cls: &str, sep: char) -> Result<(), ClassnameParsingError> {
	let mut inners_iter = cls.split('$');
	let classes_segment = inners_iter.next().ok_or(ClassnameParsingError::EmptyString)?;

	for segment in classes_segment.split(sep) {
		verify_component(segment, || cls.to_string())?
	}
	for inner in inners_iter {
		verify_component(inner, || cls.to_string())?
	}

	Ok(())
}
fn verify_component<F: FnOnce() -> String>(segment: &str, clsname: F) -> Result<(), ClassnameParsingError> {
	if segment.is_empty() {
		return Err(ClassnameParsingError::EmptyComponent(clsname()));
	}

	if let Some(err_char) = segment.chars()
		.filter_map(|c| matches!(c, '.' | ';' | '[' | '/').then(|| c))
		.next() {
		return Err(ClassnameParsingError::BadCharacter(clsname(), err_char));
	}

	Ok(())
}

