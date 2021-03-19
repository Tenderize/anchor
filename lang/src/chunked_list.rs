use solana_program::{
    account_info::AccountInfo, entrypoint::ProgramResult, instruction::AccountMeta,
    program_error::ProgramError, pubkey::Pubkey,
};

use crate::{
    Accounts, AccountsExit, AnchorDeserialize, AnchorSerialize, ToAccountInfo, ToAccountInfos,
    ToAccountMetas,
};
use std::marker::PhantomData;
#[derive(Clone, Copy, Debug, AnchorDeserialize, AnchorSerialize)]
pub struct ChunkedList<T> {
    item_size: u32,
    chunk_size: u32,  // In items
    chunk_count: u32, // capacity
    item_count: u64,
    #[borsh_skip]
    _marker: PhantomData<*const T>,
}

const MAX_ACCOUNT_SIZE: u32 = 10 * 1024 * 1024;
/*
impl<T> Default for ChunkedList<T> {
    fn default() -> Self {
        Self {
            chunk_size: MAX_ACCOUNT_SIZE / mem::size_of::<T>() as u32,
            chunk_count: 0,
            item_count: 0,
            _marker: PhantomData,
        }
    }
}*/

#[derive(Clone)]
pub struct ChunkAccount<'info, T> {
    info: AccountInfo<'info>,
    _marker: PhantomData<*const T>,
}

impl<'info, T> Accounts<'info> for ChunkAccount<'info, T> {
    #[inline(never)]
    fn try_accounts(
        program_id: &Pubkey,
        accounts: &mut &[AccountInfo<'info>],
    ) -> Result<Self, ProgramError> {
        if accounts.is_empty() {
            return Err(ProgramError::NotEnoughAccountKeys);
        }
        let account = &accounts[0];
        *accounts = &accounts[1..];
        if account.owner != program_id {
            return Err(ProgramError::Custom(1)); // todo: proper error
        }
        Ok(ChunkAccount {
            info: account.clone(),
            _marker: PhantomData,
        })
    }
}
/*
impl<'info, T> AccountsInit<'info> for ChunkAccount<'info, T>
where
    T: AccountSerialize + AccountDeserialize + Clone,
{
    #[inline(never)]
    fn try_accounts_init(
        _program_id: &Pubkey,
        accounts: &mut &[AccountInfo<'info>],
    ) -> Result<Self, ProgramError> {
        if accounts.is_empty() {
            return Err(ProgramError::NotEnoughAccountKeys);
        }
        let account = &accounts[0];
        *accounts = &accounts[1..];
        ProgramAccount::try_from_init(account)
    }
}*/

impl<'info, T> AccountsExit<'info> for ChunkAccount<'info, T> {
    fn exit(&self, _program_id: &Pubkey) -> ProgramResult {
        // TODO: write discriminant?
        /*
        let info = self.to_account_info();
        let mut data = info.try_borrow_mut_data()?;
        let dst: &mut [u8] = &mut data;
        let mut cursor = std::io::Cursor::new(dst);
        self.inner.account.try_serialize(&mut cursor)?;
        */
        Ok(())
    }
}

impl<'info, T> ToAccountMetas for ChunkAccount<'info, T> {
    fn to_account_metas(&self, is_signer: Option<bool>) -> Vec<AccountMeta> {
        let is_signer = is_signer.unwrap_or(self.info.is_signer);
        let meta = match self.info.is_writable {
            false => AccountMeta::new_readonly(*self.info.key, is_signer),
            true => AccountMeta::new(*self.info.key, is_signer),
        };
        vec![meta]
    }
}

impl<'info, T> ToAccountInfos<'info> for ChunkAccount<'info, T> {
    fn to_account_infos(&self) -> Vec<AccountInfo<'info>> {
        vec![self.info.clone()]
    }
}

impl<'info, T> ToAccountInfo<'info> for ChunkAccount<'info, T> {
    fn to_account_info(&self) -> AccountInfo<'info> {
        self.info.clone()
    }
}
