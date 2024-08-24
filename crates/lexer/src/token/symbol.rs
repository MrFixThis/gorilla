use std::borrow::Borrow;
use std::fmt::Debug;
use std::ops::Deref;

#[derive(Clone, Copy, Default, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct Symbol<'a>(&'a [u8]);

impl Debug for Symbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl Borrow<str> for Symbol<'_> {
    fn borrow(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.0) }
    }
}

impl Deref for Symbol<'_> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl AsRef<str> for Symbol<'_> {
    fn as_ref(&self) -> &str {
        Borrow::<str>::borrow(self)
    }
}

impl<'a> Symbol<'a> {
    #[inline]
    pub fn new(bytes: &'a [u8]) -> Symbol<'a> {
        Self(bytes)
    }
}
