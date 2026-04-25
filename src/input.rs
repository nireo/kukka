use memchr::memchr;

/// Input is a lightweight abstraction over &str and &[u8].
pub trait Input: Copy {
    type Item: Copy;
    type Slice: Copy + AsRef<[u8]>;
    type Iter: Iterator<Item = (usize, Self::Item)>;

    fn len(self) -> usize;
    fn split_at(self, mid: usize) -> (Self, Self);
    fn split_at_checked(self, mid: usize) -> Option<(Self, Self)>;
    fn first(self) -> Option<Self::Item>;
    fn starts_with(self, prefix: Self::Slice) -> bool;
    fn iter_indices(self) -> Self::Iter;
    fn item_len(item: Self::Item) -> usize;
    fn as_slice(self) -> Self::Slice;
    fn find_item(self, item: Self::Item) -> Option<usize>;
}

impl<'a> Input for &'a str {
    type Item = char;
    type Slice = &'a str;
    type Iter = std::str::CharIndices<'a>;

    fn len(self) -> usize {
        self.len()
    }

    fn split_at(self, mid: usize) -> (Self, Self) {
        self.split_at(mid)
    }

    fn split_at_checked(self, mid: usize) -> Option<(Self, Self)> {
        self.is_char_boundary(mid).then(|| self.split_at(mid))
    }

    fn first(self) -> Option<Self::Item> {
        self.chars().next()
    }

    fn starts_with(self, prefix: Self::Slice) -> bool {
        self.starts_with(prefix)
    }

    fn iter_indices(self) -> Self::Iter {
        self.char_indices()
    }

    fn item_len(item: Self::Item) -> usize {
        item.len_utf8()
    }

    fn as_slice(self) -> Self::Slice {
        self
    }

    fn find_item(self, item: Self::Item) -> Option<usize> {
        if item.is_ascii() {
            memchr(item as u8, self.as_bytes())
        } else {
            self.char_indices()
                .find(|(_, c)| *c == item)
                .map(|(idx, _)| idx)
        }
    }
}

impl<'a> Input for &'a [u8] {
    type Item = u8;
    type Slice = &'a [u8];
    type Iter = std::iter::Enumerate<std::iter::Copied<std::slice::Iter<'a, u8>>>;

    fn len(self) -> usize {
        self.len()
    }

    fn split_at(self, mid: usize) -> (Self, Self) {
        self.split_at(mid)
    }

    fn split_at_checked(self, mid: usize) -> Option<(Self, Self)> {
        (mid <= self.len()).then(|| self.split_at(mid))
    }

    fn first(self) -> Option<Self::Item> {
        self.first().copied()
    }

    fn starts_with(self, prefix: Self::Slice) -> bool {
        self.starts_with(prefix)
    }

    fn iter_indices(self) -> Self::Iter {
        self.iter().copied().enumerate()
    }

    fn item_len(_: Self::Item) -> usize {
        1
    }

    fn as_slice(self) -> Self::Slice {
        self
    }

    fn find_item(self, item: Self::Item) -> Option<usize> {
        memchr(item, self)
    }
}

pub trait AsciiDigit: Copy {
    fn is_ascii_digit(self) -> bool;
}

impl AsciiDigit for char {
    fn is_ascii_digit(self) -> bool {
        char::is_ascii_digit(&self)
    }
}

impl AsciiDigit for u8 {
    fn is_ascii_digit(self) -> bool {
        u8::is_ascii_digit(&self)
    }
}
