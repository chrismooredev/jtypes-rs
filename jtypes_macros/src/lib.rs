
use jtypes_core::names::*;
use jtypes_core::signature::MethodSignatureDesc;
use proc_macro::{
    TokenStream as PMTokenStream,
};
use syn::LitStr;
use quote::quote;

// only support string-based for now -- how to support otherwise?

// cls!("java/lang/String") -> Implicit
// cls!("java.lang.String") -> Implicit
// cls!(java/lang/String) -> Implicit
// cls!(java.lang.String) -> Implicit
// cls_int!("java/lang/String") -> InternalClassname
// cls_int!("java.lang.String") -> InternalClassname
// cls_int!(java/lang/String) -> InternalClassname
// cls_int!(java.lang.String) -> InternalClassname
// cls_bin!("java/lang/String") -> BinaryClassname
// cls_bin!("java.lang.String") -> BinaryClassname
// cls_bin!(java/lang/String) -> BinaryClassname
// cls_bin!(java.lang.String) -> BinaryClassname

// sig!("(IILjava/lang/String;)V") -> MethodSignature
// sig!(void (int, int, java.lang.String)) -> MethodSignature

enum ProvidedType {
    Int(InternalClassname),
    Bin(BinaryClassname),
}

fn parse_classname(item: PMTokenStream) -> ProvidedType {
    if let Ok(litstr) = syn::parse2::<LitStr>(item.into()) {
        let v = litstr.value();
        if let Ok(ic) = InternalClassname::new(v.to_owned()) {
            return ProvidedType::Int(ic);
        }
        if let Ok(bc) = BinaryClassname::new(v) {
            return ProvidedType::Bin(bc);
        }

        // TODO: a nicer error message? is this the proper way to error within a proc macro?
        panic!("expected internal (ex: java/lang/String) or binary (ex: java.lang.String) formatted classname in string literal");
    };

    todo!("non-string literal macro based internal/binary classname parsing still needed")
}

#[proc_macro]
pub fn cls(item: PMTokenStream) -> PMTokenStream {
    let ty = parse_classname(item);
    
    match ty {
        ProvidedType::Int(ic) => {
            let as_str = ic.to_str();
            quote! {
                unsafe { ::jtypes::names::InternalClassname::new_unchecked(#as_str) }.into()
            }
        },
        ProvidedType::Bin(bc) => {
            let as_str = bc.to_str();
            quote! {
                unsafe { ::jtypes::names::BinaryClassname::new_unchecked(#as_str) }.into()
            }
        }
    }.into()
}

#[proc_macro]
pub fn cls_int(item: PMTokenStream) -> PMTokenStream {
    let ty = parse_classname(item);
    
    let ic = match ty {
        ProvidedType::Int(ic) => ic,
        ProvidedType::Bin(bc) => bc.to_internal(),
    };

    let as_str = ic.to_str();
    quote! {
        unsafe { ::jtypes::names::InternalClassname::new_unchecked(#as_str) }
    }.into()
}

#[proc_macro]
pub fn cls_bin(item: PMTokenStream) -> PMTokenStream {
    let ty = parse_classname(item);
    
    let bc = match ty {
        ProvidedType::Int(ic) => ic.to_binary(),
        ProvidedType::Bin(bc) => bc,
    };

    let as_str = bc.to_str();
    quote! {
        unsafe { ::jtypes::names::BinaryClassname::new_unchecked(#as_str) }
    }.into()
}

#[proc_macro]
pub fn sig(item: PMTokenStream) -> PMTokenStream {
    if let Ok(litstr) = syn::parse2::<LitStr>(item.into()) {
        match MethodSignatureDesc::new(&litstr.value()) {
            Ok(msd) => {
                let as_str = msd.as_str();
                return quote! { unsafe { ::jtypes::signature::MethodSignatureDesc::new_unchecked(#as_str) } }.into()
            },
            Err(e) => {
                panic!("string macro arg not a valid method signature: {:?}", e);
            }
        }
    }
    
    todo!("non-string literal macro based method signature still needed");
}