
use std::borrow::Cow;
use std::fmt;
use std::ops::Deref;

use cesu8str::{Cesu8Str, Variant};

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

/// A classname specifier in internal format (ex: `java/lang/Object`)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternalClassname(Cesu8Str<'static>);

/// A classname specifier in binary format (ex: `java.lang.Object`)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinaryClassname(Cesu8Str<'static>);

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct GenericComponents<S> {
	/// Package + terminal class name
	base_class: Vec<S>,

	/// Arbitrary number of nested inner classes
	inner_classes: Vec<S>,
}
// pub type Components = GenericComponents<Cesu8Str<'static>>;
pub type Components<'a> = GenericComponents<Cesu8Str<'a>>;



impl InternalClassname {

	/// Creates a new class name using internal format (ex: `java/lang/Object`). Returns an error if it is invalid or empty.
	pub fn new<IC: Into<Cow<'static, str>>>(cls: IC) -> Result<InternalClassname, ClassnameParsingError> {
		let cls: Cow<'static, str> = cls.into();

		verify_classname_str(&cls, '/')?;

		let cls = Cesu8Str::from_utf8(cls, Variant::Java);

		Ok(InternalClassname(cls))
	}

	pub fn new_with_cesu<'s, C: Into<Cesu8Str<'s>>>(cls: C) -> Result<InternalClassname, ClassnameParsingError> {
		let cls: Cesu8Str<'s> = cls.into();

		let as_str = cls.to_str();
		verify_classname_str(&as_str, '/')?;

		let cls = cls.to_variant(Variant::Java);

		Ok(InternalClassname(cls.into_owned()))
	}

	/// Creates a new class name using internal format (ex: `java/lang/Object`).
	/// 
	/// # Safety
	/// The internal name must be valid, otherwise unexpected behavior may occur in other functions.
	/// 
	/// Validation checking can be done with `InternalClassname::new(...)`
	pub unsafe fn new_unchecked<'s, IC: Into<Cow<'s, str>>>(cls: IC) -> InternalClassname {
		InternalClassname(Cesu8Str::from_utf8(cls, Variant::Java).into_owned())
	}

	pub fn into_inner(self) -> Cesu8Str<'static> {
		self.0
	}

	pub fn to_binary(&self) -> BinaryClassname {
		BinaryClassname::from(self)
	}
	pub fn to_components(&self) -> Components<'static> {
		Components::from(self.clone())
	}
	pub fn as_components(&self) -> Components<'_> {
		Components::from(self)
	}
}
impl BinaryClassname {
	
	/// Creates a new class name using binary format (ex: `java.lang.Object`). Returns an error if it is invalid or empty.
	pub fn new<IC: Into<Cow<'static, str>>>(cls: IC) -> Result<BinaryClassname, ClassnameParsingError> {
		let cls: Cow<'static, str> = cls.into();
		
		verify_classname_str(&cls, '.')?;
		
		Ok(BinaryClassname(Cesu8Str::from_utf8(cls, Variant::Java)))
	}
	
	pub fn into_inner(self) -> Cesu8Str<'static> {
		self.0
	}

	pub fn to_internal(&self) -> InternalClassname {
		InternalClassname::from(self)
	}
	pub fn to_components(&self) -> Components<'static> {
		Components::from(self.clone())
	}
	pub fn as_components(&self) -> Components<'_> {
		Components::from(self)
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
}
impl<S> GenericComponents<S> {
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
impl<'a> GenericComponents<Cesu8Str<'a>> {
	/// Used internally to convert internal and binary class descriptors into individual components
	fn split_from_string<'s: 'a>(string: &'s Cesu8Str<'a>, sep: char) -> GenericComponents<Cesu8Str<'a>> {
		// Note: Inner classes have a dollar sign prefix, specified after the base class
		// As it uses the same separater, it does not have to be specially treated here

		// TODO: see how this interacts with arrays/etc. Is that even necessary here?

		let raw = string.as_bytes();

		// let string = string.as_ref();
		let mut inner_iter = raw.split(|&b| b == b'$');
		let base_class = inner_iter.next().expect("there should be at least one base class");
		let base_class: Vec<_> = base_class.split(|&b| b == sep as u8)
			.map(|s| Cesu8Str::from_cesu8(s, Variant::Java).unwrap())
			.collect();
		let inner_class = inner_iter
			.map(|s| Cesu8Str::from_cesu8(s, Variant::Java).unwrap())
			.collect();
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
	let _gcstr: GenericComponents<Cesu8Str<'_>> = (&intclsname).into();
}

// Convert between types

// InternalClassname <-> BinaryClassname
impl From<&InternalClassname> for BinaryClassname {
	fn from(int: &InternalClassname) -> BinaryClassname {
		// Note: Inner classes have a dollar sign prefix, specified after the base class
		// As it uses the same seperator, it does not have to be specially treated here

		// TODO: see how this interacts with arrays/etc. Is that even necessary here?
		// TODO: provide implementation that changes underlying str? (eg no alloc) would need unsafe, likely not worth the effort for any usecases
		
		if int.0.as_bytes().contains(&b'/') {
			BinaryClassname(Cesu8Str::from_utf8(int.0.to_str().split('/').collect::<Vec<&str>>().join("."), Variant::Java))
		} else {
			BinaryClassname(int.0.clone())
		}
	}
}
impl From<&BinaryClassname> for InternalClassname {
	fn from(bin: &BinaryClassname) -> InternalClassname {
		// Note: Inner classes have a dollar sign prefix, specified after the base class
		// As it uses the same separater, it does not have to be specially treated here

		// TODO: see how this interacts with arrays/etc. Is that even necessary here?
		if bin.0.as_bytes().contains(&b'.') {
			InternalClassname(Cesu8Str::from_utf8(bin.0.to_str().split('.').collect::<Vec<&str>>().join("/"), Variant::Java))
		} else {
			InternalClassname(bin.0.clone())
		}
	}
}
impl From<InternalClassname> for BinaryClassname { fn from(item: InternalClassname) -> Self { (&item).into() } }
impl From<BinaryClassname> for InternalClassname { fn from(item: BinaryClassname) -> Self { (&item).into() } }

// InternalClassname <-> Components
impl From<InternalClassname> for GenericComponents<Cesu8Str<'static>> {
	fn from(int: InternalClassname) -> Self {
		let GenericComponents {
			base_class,
			inner_classes,
		} = GenericComponents::split_from_string(&int.0, '/');
		GenericComponents {
			base_class: base_class.into_iter().map(Cesu8Str::into_owned).collect(),
			inner_classes: inner_classes.into_iter().map(Cesu8Str::into_owned).collect(),
		}
	}
}
impl<'a> From<&'a InternalClassname> for GenericComponents<Cesu8Str<'a>> {
	fn from(int: &'a InternalClassname) -> Self {
		Self::split_from_string(&int.0, '/')
	}
}
impl<S: AsRef<str>> From<&GenericComponents<S>> for InternalClassname {
	fn from(comp: &GenericComponents<S>) -> InternalClassname {
		InternalClassname(Cesu8Str::from_utf8(comp.combine_to_string("/"), Variant::Java))
	}
}

impl From<InternalClassname> for GenericComponents<String> { fn from(item: InternalClassname) -> Self { item.into() } }
impl<S: AsRef<str>> From<GenericComponents<S>> for InternalClassname { fn from(item: GenericComponents<S>) -> Self { (&item).into() } }

// BinaryClassname <-> Components
impl From<BinaryClassname> for GenericComponents<Cesu8Str<'static>> {
	fn from(int: BinaryClassname) -> Self {
		let GenericComponents {
			base_class,
			inner_classes,
		} = GenericComponents::split_from_string(&int.0, '.');
		GenericComponents {
			base_class: base_class.into_iter().map(Cesu8Str::into_owned).collect(),
			inner_classes: inner_classes.into_iter().map(Cesu8Str::into_owned).collect(),
		}
	}
}
impl<'a> From<&'a BinaryClassname> for GenericComponents<Cesu8Str<'a>> {
	fn from(int: &'a BinaryClassname) -> Self {
		Self::split_from_string(&int.0, '.')
	}
}
impl<S: AsRef<str>> From<&GenericComponents<S>> for BinaryClassname {
	fn from(comp: &GenericComponents<S>) -> Self {
		BinaryClassname(Cesu8Str::from_utf8(comp.combine_to_string("."), Variant::Java))
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
				GenericComponents::<Cesu8Str>::from(self) == GenericComponents::<Cesu8Str>::from(other)
			}
		}
		#[allow(clippy::cmp_owned)]
		impl PartialEq<$first> for $second {
			fn eq(&self, other: &$first) -> bool {
				GenericComponents::<Cesu8Str>::from(self) == GenericComponents::<Cesu8Str>::from(other)
			}
		}
		#[allow(clippy::cmp_owned)]
		impl PartialOrd<$second> for $first {
			fn partial_cmp(&self, other: &$second) -> Option<std::cmp::Ordering> {
				GenericComponents::<Cesu8Str>::from(self).partial_cmp(&GenericComponents::<Cesu8Str>::from(other))
			}
		}
		#[allow(clippy::cmp_owned)]
		impl PartialOrd<$first> for $second {
			fn partial_cmp(&self, other: &$first) -> Option<std::cmp::Ordering> {
				GenericComponents::<Cesu8Str>::from(self).partial_cmp(&GenericComponents::<Cesu8Str>::from(other))
			}
		}
	}
}

macro_rules! impl_classname_eq_one_components {
	($first: ty) => {
		#[allow(clippy::cmp_owned)]
		impl PartialEq<GenericComponents<Cesu8Str<'_>>> for $first {
			fn eq(&self, other: &GenericComponents<Cesu8Str<'_>>) -> bool {
				GenericComponents::<Cesu8Str>::from(self) == *other
			}
		}
		#[allow(clippy::cmp_owned)]
		impl PartialEq<$first> for GenericComponents<Cesu8Str<'_>> {
			fn eq(&self, other: &$first) -> bool {
				GenericComponents::<Cesu8Str>::from(other) == *self
			}
		}
		
		#[allow(clippy::cmp_owned)]
		impl PartialOrd<GenericComponents<Cesu8Str<'_>>> for $first {
			fn partial_cmp(&self, other: &GenericComponents<Cesu8Str<'_>>) -> Option<std::cmp::Ordering> {
				GenericComponents::<Cesu8Str>::from(self).partial_cmp(other)
			}
		}
		#[allow(clippy::cmp_owned)]
		impl PartialOrd<$first> for GenericComponents<Cesu8Str<'_>> {
			fn partial_cmp(&self, other: &$first) -> Option<std::cmp::Ordering> {
				self.partial_cmp(&GenericComponents::<Cesu8Str>::from(other))
			}
		}
	}
}

// InternalClassname <-> BinaryClassname
impl_classname_eq_no_components!(InternalClassname, BinaryClassname);
// InternalClassname <-> Components
impl_classname_eq_one_components!(InternalClassname);
// BinaryClassname <-> Components
impl_classname_eq_one_components!(BinaryClassname);


macro_rules! impl_common_traits {
	($classtype: ty) => {
		impl fmt::Display for $classtype {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				fmt::Display::fmt(&self.0, f)
			}
		}

		impl From<$classtype> for String {
			fn from(ic: $classtype) -> String {
				ic.0.into_str().into_owned()
			}
		}
	}
}


impl Deref for InternalClassname { type Target = Cesu8Str<'static>; fn deref(&self) -> &Self::Target { &self.0 } }
impl Deref for BinaryClassname { type Target = Cesu8Str<'static>; fn deref(&self) -> &Self::Target { &self.0 } }
impl_common_traits!(InternalClassname);
impl_common_traits!(BinaryClassname);

// impl From<&InternalClassname> for InternalClassname {
// 	fn from(ic: &InternalClassname) -> InternalClassname {
// 		ic.clone()
// 	}
// }
// impl From<&BinaryClassname> for BinaryClassname {
// 	fn from(ic: &BinaryClassname) -> BinaryClassname {
// 		ic.clone()
// 	}
// }

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

