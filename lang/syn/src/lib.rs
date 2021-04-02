//! DSL syntax tokens.

#[cfg(feature = "idl")]
use crate::idl::{IdlAccount, IdlAccountItem, IdlAccounts};
use anyhow::Result;
#[cfg(feature = "idl")]
use heck::MixedCase;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::{
    collections::HashMap,
    ops::{Deref, DerefMut, Range},
};
use syn::{
    punctuated::Punctuated, spanned::Spanned, Fields, Generics, Ident, ItemStruct, LitByteStr,
    LitInt, LitStr, Token, Variant,
};

pub mod accounts;
pub mod codegen;
#[cfg(feature = "hash")]
pub mod hash;
#[cfg(not(feature = "hash"))]
pub(crate) mod hash;
#[cfg(feature = "idl")]
pub mod idl;
pub mod parser;

#[derive(Debug)]
pub struct Program {
    pub state: Option<State>,
    pub ixs: Vec<Ix>,
    pub name: syn::Ident,
    pub program_mod: syn::ItemMod,
}

// State struct singleton.
#[derive(Debug)]
pub struct State {
    pub name: String,
    pub strct: syn::ItemStruct,
    pub ctor_and_anchor: Option<(syn::ImplItemMethod, syn::Ident)>,
    pub impl_block_and_methods: Option<(syn::ItemImpl, Vec<StateIx>)>,
    pub interfaces: Option<Vec<StateInterface>>,
}

#[derive(Debug)]
pub struct StateIx {
    pub raw_method: syn::ImplItemMethod,
    pub ident: syn::Ident,
    pub args: Vec<IxArg>,
    pub anchor_ident: syn::Ident,
    // True if there exists a &self on the method.
    pub has_receiver: bool,
}

#[derive(Debug)]
pub struct StateInterface {
    pub trait_name: String,
    pub methods: Vec<StateIx>,
}

#[derive(Debug)]
pub struct Ix {
    pub raw_method: syn::ItemFn,
    pub ident: syn::Ident,
    pub args: Vec<IxArg>,
    // The ident for the struct deriving Accounts.
    pub anchor_ident: syn::Ident,
}

#[derive(Debug)]
pub struct IxArg {
    pub name: proc_macro2::Ident,
    pub raw_arg: syn::PatType,
}

#[derive(Debug)]
pub enum ErrorItem {
    Enum(ErrorEnum),
    Struct(ErrorStruct),
}

#[derive(Debug)]
pub struct ErrorEnum {
    // pub name: String,
    pub ident: syn::Ident,
    pub generics: Generics,
    pub variants: Vec<ErrorVariant>,
    pub custom_code_range: Option<Range<u32>>,
}

#[derive(Debug)]
pub struct ErrorStruct {
    pub ident: Ident,
    pub fields: Fields,
    pub target: ErrorTarget,
    pub message: Option<ErrorVariantMessage>,
    pub raw: ItemStruct,
}

#[derive(Debug)]
pub struct ErrorVariant {
    pub ident: Ident,
    pub fields: Fields,
    pub target: ErrorTarget,
    pub message: Option<ErrorVariantMessage>,
    pub raw: Variant,
}

#[derive(Debug)]
pub enum ErrorTarget {
    Custom(Option<LitInt>),
    Alias(Ident),
    From { field_index: usize },
}

#[derive(Debug)]
pub struct ErrorVariantMessage {
    pub format: LitStr,
    pub positinal_args: TokenStream,
    pub named_args: TokenStream,
}

pub struct WithContext<T, C> {
    inner: T,
    context: C,
}

impl<T, C> WithContext<T, C> {
    pub fn new(inner: T, context: C) -> Self {
        Self { inner, context }
    }

    pub fn context(&self) -> C
    where
        C: Clone,
    {
        self.context.clone()
    }

    pub fn into_inner(self) -> T {
        self.inner
    }
}

impl<T, C> Deref for WithContext<T, C> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T, C> DerefMut for WithContext<T, C> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

type WithSpan<T> = WithContext<T, Span>;

impl<T> Spanned for WithSpan<T> {
    fn span(&self) -> Span {
        self.context()
    }
}
