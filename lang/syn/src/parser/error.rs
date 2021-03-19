use std::{convert::TryFrom, ops::Range};

use crate::{ErrorEnum, ErrorItem, ErrorStruct, ErrorTarget, ErrorVariant, ErrorVariantMessage};
use syn::{
    parse::{Parse, ParseStream},
    Item, ItemStruct, ItemUnion, Lit, Meta, MetaList, NestedMeta,
};
use syn::{DeriveInput, Error, ItemEnum, Result, Variant};

impl Parse for ErrorItem {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::try_from(<DeriveInput as Parse>::parse(input)?)
    }
}

impl TryFrom<DeriveInput> for ErrorItem {
    type Error = Error;

    fn try_from(input: DeriveInput) -> Result<Self> {
        match input.into() {
            Item::Struct(item_struct) => Ok(ErrorItem::Struct(ErrorStruct::try_from(item_struct)?)),
            Item::Enum(item_enum) => Ok(ErrorItem::Enum(ErrorEnum::try_from(item_enum)?)),
            Item::Union(ItemUnion { union_token, .. }) => Err(Error::new_spanned(
                union_token,
                "Union error is not supported",
            )),
            _ => unreachable!(),
        }
    }
}

impl TryFrom<ItemStruct> for ErrorStruct {
    type Error = Error;

    fn try_from(item_struct: ItemStruct) -> Result<Self> {
        Err(Error::new_spanned(
            item_struct.struct_token,
            "Not implemented yet",
        ))
    }
}

impl TryFrom<ItemEnum> for ErrorEnum {
    type Error = Error;

    fn try_from(item_enum: ItemEnum) -> Result<Self> {
        let mut custom_code_range = None;
        for attr in item_enum.attrs {
            let attr = attr.parse_meta()?;
            let ident = attr
                .path()
                .get_ident()
                .ok_or_else(|| Error::new_spanned(attr.path(), "Must be ident"))?;
            match ident.to_string().as_ref() {
                "range" => {
                    if custom_code_range.is_some() {
                        return Err(Error::new_spanned(ident, "Duplicated attribute"));
                    }
                    custom_code_range = Some(parse_range(attr)?);
                }
                s => return Err(Error::new_spanned(ident, "Unknown attribute")),
            }
        }
        let ident = item_enum.ident.clone();
        let variants = item_enum
            .variants
            .into_iter()
            .map(<ErrorVariant as TryFrom<Variant>>::try_from)
            .collect::<Result<Vec<ErrorVariant>>>()?;

        Ok(ErrorEnum {
            generics: item_enum.generics.clone(),
            ident,
            variants,
            custom_code_range,
        })
    }
}

fn parse_range(attr: Meta) -> Result<Range<u32>> {
    match attr {
        Meta::List(MetaList { nested, .. }) => {
            /*let it = nested.iter();
            let low = if let Some(item) = it.next() {
                if NestedMeta::Lit(Lit(lit)) = item {
                    lit
                } else {
                    return Err(Error::new_spanned(item, "Must be an integer literal"));
                }
            } else {
                return Err(Error::new_spanned(attr, "Must have exactly 2 arguments"));
            };*/

            Err(Error::new_spanned(nested, "Must be like #[range(0, 100)]"))
        }
        _ => Err(Error::new_spanned(attr, "Must be like #[range(0, 100)]")),
    }
}

impl TryFrom<Variant> for ErrorVariant {
    type Error = Error;

    fn try_from(variant: Variant) -> Result<Self> {
        Ok(ErrorVariant {
            ident: variant.ident.clone(),
            fields: variant.fields.clone(),
            target: ErrorTarget::Custom(None),
            message: None,
            raw: variant.clone(),
        })
    }
}
