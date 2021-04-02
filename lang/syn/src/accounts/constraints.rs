use std::{collections::BTreeSet, convert::TryFrom, iter::FromIterator};

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    bracketed,
    ext::IdentExt,
    parse::{Parse, ParseStream},
    parse2,
    spanned::Spanned,
    token::Token,
    Path,
};
use syn::{
    punctuated::Punctuated, token::Comma, Attribute, Error, Expr, Ident, LitStr, Result, Token,
};

use crate::{WithContext, WithSpan};

use super::field::Ty;

const INIT_NAME: &str = "init";
const MUT_NAME: &str = "mut";
const SIGNER_NAME: &str = "signer";
const SEEDS_NAME: &str = "seeds";
const BELONGS_TO_NAME: &str = "belongs_to";
const OWNER_NAME: &str = "owner";
const RENT_EXEMPT_NAME: &str = "rent_exempt";
const EXPR_NAME: &str = "expr";

pub enum Constraint {
    Init(WithSpan<ConstraintInit>),
    Mut(WithSpan<ConstraintMut>),
    Signer(WithSpan<ConstraintSigner>),
    Seeds(WithSpan<ConstraintSeeds>),
    BelongsTo(WithSpan<ConstraintBelongsTo>),
    Owner(WithSpan<ConstraintOwner>),
    RentExempt(WithSpan<ConstraintRentExempt>),
    Expr(Box<WithSpan<ConstraintExpr>>),
}

impl Spanned for Constraint {
    fn span(&self) -> Span {
        match self {
            Constraint::Init(c) => c.span(),
            Constraint::Mut(c) => c.span(),
            Constraint::Signer(c) => c.span(),
            Constraint::Seeds(c) => c.span(),
            Constraint::BelongsTo(c) => c.span(),
            Constraint::Owner(c) => c.span(),
            Constraint::RentExempt(c) => c.span(),
            Constraint::Expr(c) => c.span(),
        }
    }
}

// TODO: Constraint trait

impl Parse for Constraint {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.fork().call(Ident::parse_any)?;
        match ident.to_string().as_str() {
            INIT_NAME => Ok(Self::Init(Parse::parse(input)?)),
            MUT_NAME => Ok(Self::Mut(Parse::parse(input)?)),
            SIGNER_NAME => Ok(Self::Signer(Parse::parse(input)?)),
            SEEDS_NAME => Ok(Self::Seeds(Parse::parse(input)?)),
            BELONGS_TO_NAME => Ok(Self::BelongsTo(Parse::parse(input)?)),
            OWNER_NAME => Ok(Self::Owner(Parse::parse(input)?)),
            RENT_EXEMPT_NAME => Ok(Self::RentExempt(Parse::parse(input)?)),
            EXPR_NAME => Ok(Self::Expr(Box::new(Parse::parse(input)?))),
            _ => Err(Error::new_spanned(ident, "Unknown attribute")),
        }
    }
}

#[derive(Debug)]
pub struct ConstraintInit;

impl Parse for WithSpan<ConstraintInit> {
    fn parse(input: ParseStream) -> Result<Self> {
        let span = input.span();
        let ident: Ident = input.parse()?;
        if ident != INIT_NAME {
            return Err(Error::new(span, "Expected init"));
        }
        Ok(Self::new(ConstraintInit, span))
    }
}

#[derive(Debug)]
pub struct ConstraintMut;

impl Parse for WithSpan<ConstraintMut> {
    fn parse(input: ParseStream) -> Result<Self> {
        let span = input.span();
        let ident = input.call(Ident::parse_any)?;
        if ident != MUT_NAME {
            return Err(Error::new(span, "Expected mut"));
        }
        Ok(Self::new(ConstraintMut, span))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstraintSigner;

impl Parse for WithSpan<ConstraintSigner> {
    fn parse(input: ParseStream) -> Result<Self> {
        let span = input.span();
        let ident: Ident = input.parse()?;
        if ident != SIGNER_NAME {
            return Err(Error::new(span, "Expected signer"));
        }
        Ok(Self::new(ConstraintSigner, span))
    }
}

#[derive(Debug)]
pub struct ConstraintSeeds {
    pub seeds: Punctuated<Expr, Token![,]>,
}

impl Parse for WithSpan<ConstraintSeeds> {
    fn parse(input: ParseStream) -> Result<Self> {
        let start_span = input.span();
        let ident: Ident = input.parse()?;
        if ident != SEEDS_NAME {
            return Err(Error::new(start_span, "Expected seeds"));
        }
        input.parse::<Token![=]>()?;
        let content;
        let bracket = bracketed!(content in input);
        Ok(Self::new(
            ConstraintSeeds {
                seeds: content.parse_terminated(Expr::parse)?,
            },
            start_span.join(bracket.span).unwrap_or(start_span),
        ))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstraintBelongsTo {
    pub join_target: Ident,
}

impl Parse for WithSpan<ConstraintBelongsTo> {
    fn parse(input: ParseStream) -> Result<Self> {
        let start_span = input.span();
        let ident: Ident = input.parse()?;
        if ident != BELONGS_TO_NAME {
            return Err(Error::new(start_span, "Expected belongs_to"));
        }
        input.parse::<Token![=]>()?;
        let end_span = input.span();
        Ok(Self::new(
            ConstraintBelongsTo {
                join_target: input.parse()?,
            },
            start_span.join(end_span).unwrap_or(start_span),
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConstraintOwner {
    Program,
    Value(LitStr),
    Skip,
}

impl Parse for WithSpan<ConstraintOwner> {
    fn parse(input: ParseStream) -> Result<Self> {
        let start_span = input.span();
        let ident: Ident = input.parse()?;
        if ident != OWNER_NAME {
            return Err(Error::new(start_span, "Expected owner"));
        }

        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;

            if input.peek(LitStr) {
                let lit: LitStr = input.parse()?;
                let lit_span = lit.span();
                return Ok(Self::new(
                    ConstraintOwner::Value(lit),
                    start_span.join(lit_span).unwrap_or(start_span),
                ));
            }

            let ident: Ident = input.parse()?;
            let span = start_span.join(ident.span()).unwrap_or(start_span);

            match ident.to_string().as_str() {
                "skip" => Ok(Self::new(ConstraintOwner::Skip, span)),
                "program" => Ok(Self::new(ConstraintOwner::Program, span)),
                _ => Err(Error::new(span, "Expected skip or program")),
            }
        } else {
            Ok(Self::new(ConstraintOwner::Program, start_span))
        }
    }
}

#[derive(Debug)]
pub enum ConstraintRentExempt {
    Enforce,
    Skip,
}

impl Parse for WithSpan<ConstraintRentExempt> {
    fn parse(input: ParseStream) -> Result<Self> {
        let start_span = input.span();
        let ident: Ident = input.parse()?;
        if ident != RENT_EXEMPT_NAME {
            return Err(Error::new(start_span, "Expected rent_exempt"));
        }

        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;

            let ident: Ident = input.parse()?;
            let span = start_span.join(ident.span()).unwrap_or(start_span);

            match ident.to_string().as_str() {
                "skip" => Ok(Self::new(ConstraintRentExempt::Skip, span)),
                _ => Err(Error::new(span, "Expected skip")),
            }
        } else {
            Ok(Self::new(ConstraintRentExempt::Enforce, start_span))
        }
    }
}

#[derive(Debug)]
pub struct ConstraintExpr(Expr);

impl Parse for WithSpan<ConstraintExpr> {
    fn parse(input: ParseStream) -> Result<Self> {
        let start_span = input.span();
        let ident: Ident = input.parse()?;
        if ident != EXPR_NAME {
            return Err(Error::new(start_span, "Expected expr"));
        }
        input.parse::<Token![=]>()?;
        let expr: Expr = input.parse()?;
        let expr_span = expr.span();
        Ok(Self::new(
            ConstraintExpr(expr),
            start_span.join(expr_span).unwrap_or(start_span),
        ))
    }
}

impl<'a> ToTokens for ConstraintExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}
