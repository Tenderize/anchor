use std::{
    convert::{TryFrom, TryInto},
    fmt::Display,
};

use enum_dispatch::enum_dispatch;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Attribute, Error, Field, Ident, Path, Result, Type};
mod composite;
mod plain;

use self::{composite::CompositeField, plain::PlainField};

#[enum_dispatch]
pub trait MetaAccountFieldGenerator {
    fn to_account_metas(&self) -> TokenStream;
    fn field(&self) -> TokenStream;
}
#[enum_dispatch]
pub trait AccountFieldGenerator: MetaAccountFieldGenerator {
    fn deser(&self) -> TokenStream;
    fn access_checks(&self) -> TokenStream;
    fn to_account_metas(&self) -> TokenStream;
    fn to_account_infos(&self) -> TokenStream;
    fn on_save(&self) -> TokenStream;
}

#[enum_dispatch(MetaAccountFieldGenerator, AccountFieldGenerator)]
#[derive(Debug)]
pub enum AccountField {
    // Use a `String` instead of the `AccountsStruct` because all
    // accounts structs aren't visible to a single derive macro.
    //
    // When we need the global context, we fill in the String with the
    // appropriate values. See, `account_tys` as an example.
    CompositeField, // Composite
    PlainField,     // Primitive
}

impl AccountField {
    pub fn ident(&self) -> Ident {
        match self {
            AccountField::CompositeField(f) => f.ident.clone(),
            AccountField::PlainField(f) => f.ident.clone(),
        }
    }
}
/*
    pub fn deser(&self) -> TokenStream {
        match self {
            AccountField::CompositeField(f) => f.deser(),
            AccountField::PlainField(f) => f.deser(),
        }
    }

    pub fn meta_account_metas(&self) -> TokenStream {
        match self {
            AccountField::CompositeField(f) => f.meta_account_metas(),
            AccountField::PlainField(f) => f.meta_account_metas(),
        }
    }

    pub fn meta_field(&self) -> TokenStream {
        match self {
            AccountField::CompositeField(f) => f.meta_field(),
            AccountField::PlainField(f) => f.meta_field(),
        }
    }

    pub fn access_checks(&self) -> TokenStream {
        match self {
            AccountField::CompositeField(f) => f.access_checks(),
            AccountField::PlainField(f) => f.access_checks(),
        }
    }

    pub fn to_acc_infos(&self) -> TokenStream {
        match self {
            AccountField::CompositeField(f) => f.to_acc_infos(),
            AccountField::PlainField(f) => f.to_acc_infos(),
        }
    }

    pub fn on_save(&self) -> TokenStream {
        match self {
            AccountField::CompositeField(f) => f.on_save(),
            AccountField::PlainField(f) => f.on_save(),
        }
    }
}*/

impl TryFrom<Field> for AccountField {
    type Error = Error;

    fn try_from(field: Field) -> Result<Self> {
        Ok(match Ty::try_from(field.ty.clone())? {
            Ty::Compound(_) => AccountField::CompositeField(field.try_into()?),
            _ => AccountField::PlainField(field.try_into()?),
        })
    }
}

// An account in the accounts struct.

/*
impl PlainField {
    pub fn typed_ident(&self) -> proc_macro2::TokenStream {
        let name = &self.ident;

        let ty = match &self.ty {
            Ty::AccountInfo => quote! { AccountInfo },
            Ty::ProgramState(ty) => {
                let account = &ty.account_ident;
                quote! {
                    ProgramState<#account>
                }
            }
            Ty::ProgramAccount(ty) => {
                let account = &ty.account_ident;
                quote! {
                    ProgramAccount<#account>
                }
            }
            Ty::CpiAccount(ty) => {
                let account = &ty.account_ident;
                quote! {
                    CpiAccount<#account>
                }
            }
            Ty::Sysvar(ty) => {
                let account = match ty {
                    SysvarTy::Clock => quote! {Clock},
                    SysvarTy::Rent => quote! {Rent},
                    SysvarTy::EpochSchedule => quote! {EpochSchedule},
                    SysvarTy::Fees => quote! {Fees},
                    SysvarTy::RecentBlockHashes => quote! {RecentBlockHashes},
                    SysvarTy::SlotHashes => quote! {SlotHashes},
                    SysvarTy::SlotHistory => quote! {SlotHistory},
                    SysvarTy::StakeHistory => quote! {StakeHistory},
                    SysvarTy::Instructions => quote! {Instructions},
                    SysvarTy::Rewards => quote! {Rewards},
                };
                quote! {
                    Sysvar<#account>
                }
            }
            Ty::ChunkAccount(ty) => {
                let item = &ty.item_ident;
                quote! { ChunkAccount<#item> }
            }
        };

        quote! {
            #name: #ty
        }
    }
}
*/
const PROGRAM_STATE_TY: &str = "ProgramState";
const PROGRAM_ACCOUNT_TY: &str = "ProgramAccount";
const CPI_ACCOUNT_TY: &str = "CpiAccount";
const SYSVAR_TY: &str = "Sysvar";
const ACCOUNT_INFO_TY: &str = "AccountInfo";
const CHUNK_ACCOUNT_TY: &str = "ChunkAccount";

// A type of an account field.
#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    AccountInfo,
    ProgramState(ProgramStateTy),
    ProgramAccount(ProgramAccountTy),
    CpiAccount(CpiAccountTy),
    Sysvar(SysvarTy),
    ChunkAccount(ChunkAccountTy),
    Compound(Box<Type>),
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Ty::AccountInfo => "AccountInfo",
            Ty::ProgramState(_) => "ProgramState",
            Ty::ProgramAccount(_) => "ProgramAccount",
            Ty::CpiAccount(_) => "CpiAccount",
            Ty::Sysvar(_) => "Sysvar",
            Ty::ChunkAccount(_) => "ChunkAccount",
            Ty::Compound(_) => "Compound",
        })
    }
}

impl TryFrom<Type> for Ty {
    type Error = Error;

    fn try_from(value: Type) -> Result<Self> {
        Ok(match value {
            Type::Path(ty_path) => {
                if ty_path.path.leading_colon.is_none() && ty_path.path.segments.len() == 1 {
                    match ty_path.path.segments[0].ident.to_string().as_str() {
                        PROGRAM_STATE_TY => Self::ProgramState(ty_path.path.try_into()?),
                        PROGRAM_ACCOUNT_TY => Self::ProgramAccount(ty_path.path.try_into()?),
                        CPI_ACCOUNT_TY => Self::CpiAccount(ty_path.path.try_into()?),
                        SYSVAR_TY => Self::Sysvar(ty_path.path.try_into()?),
                        ACCOUNT_INFO_TY => Self::AccountInfo, // TODO check parameters
                        CHUNK_ACCOUNT_TY => Self::ChunkAccount(ty_path.path.try_into()?),
                        _ => Self::Compound(Box::new(Type::Path(ty_path))),
                    }
                } else {
                    Self::Compound(Box::new(Type::Path(ty_path)))
                }
            }
            value => Self::Compound(Box::new(value)),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SysvarTy {
    Clock,
    Rent,
    EpochSchedule,
    Fees,
    RecentBlockHashes,
    SlotHashes,
    SlotHistory,
    StakeHistory,
    Instructions,
    Rewards,
}

impl TryFrom<Path> for SysvarTy {
    type Error = Error;

    fn try_from(path: Path) -> Result<Self> {
        let account_ident = match &path.segments[0].arguments {
            syn::PathArguments::AngleBracketed(args) => {
                // Expected: <'info, MyType>.
                if args.args.len() != 2 {
                    return Err(Error::new_spanned(path, "Invalid Sysvar"));
                }
                match &args.args[1] {
                    syn::GenericArgument::Type(syn::Type::Path(ty_path)) => {
                        // TODO: allow segmented paths.
                        assert!(ty_path.path.segments.len() == 1);
                        let path_segment = &ty_path.path.segments[0];
                        path_segment.ident.clone()
                    }
                    _ => return Err(Error::new_spanned(path, "Invalid Sysvar")),
                }
            }
            _ => return Err(Error::new_spanned(path, "Invalid Sysvar")),
        };
        match account_ident.to_string().as_str() {
            "Clock" => Ok(Self::Clock),
            "Rent" => Ok(Self::Rent),
            "EpochSchedule" => Ok(Self::EpochSchedule),
            "Fees" => Ok(Self::Fees),
            "RecentBlockhashes" => Ok(Self::RecentBlockHashes),
            "SlotHashes" => Ok(Self::SlotHashes),
            "SlotHistory" => Ok(Self::SlotHistory),
            "StakeHistory" => Ok(Self::StakeHistory),
            "Instructions" => Ok(Self::Instructions),
            "Rewards" => Ok(Self::Rewards),
            _ => Err(Error::new_spanned(account_ident, "Invalid Sysvar")),
        }
    }
}

fn parse_account(path: Path) -> Result<Ident> {
    match &path.segments[0].arguments {
        syn::PathArguments::AngleBracketed(args) => {
            // Expected: <'info, MyType>.
            if args.args.len() != 2 {
                return Err(Error::new_spanned(path, "Invalid Sysvar"));
            }
            match &args.args[1] {
                syn::GenericArgument::Type(syn::Type::Path(ty_path)) => {
                    // TODO: allow segmented paths.
                    assert!(ty_path.path.segments.len() == 1);
                    let path_segment = &ty_path.path.segments[0];
                    Ok(path_segment.ident.clone())
                }
                _ => Err(Error::new_spanned(path, "Invalid ProgramAccount")),
            }
        }
        _ => Err(Error::new_spanned(path, "Invalid ProgramAccount")),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProgramStateTy {
    pub account_ident: Ident,
}

impl TryFrom<Path> for ProgramStateTy {
    type Error = Error;

    fn try_from(value: Path) -> Result<Self> {
        Ok(Self {
            account_ident: parse_account(value)?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProgramAccountTy {
    // The struct type of the account.
    pub account_ident: Ident,
}

impl TryFrom<Path> for ProgramAccountTy {
    type Error = Error;

    fn try_from(value: Path) -> Result<Self> {
        Ok(Self {
            account_ident: parse_account(value)?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CpiAccountTy {
    // The struct type of the account.
    pub account_ident: syn::Ident,
}

impl TryFrom<Path> for CpiAccountTy {
    type Error = Error;

    fn try_from(value: Path) -> Result<Self> {
        Ok(Self {
            account_ident: parse_account(value)?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ChunkAccountTy {
    // The struct type of the account.
    pub item_ident: syn::Ident,
}

impl TryFrom<Path> for ChunkAccountTy {
    type Error = Error;

    fn try_from(value: Path) -> Result<Self> {
        Ok(Self {
            item_ident: parse_account(value)?,
        })
    }
}
