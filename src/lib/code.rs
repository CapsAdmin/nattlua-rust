pub struct Code<'a> {
    pub buffer: &'a str,
    pub name: &'a str,
}

impl<'a> Code<'a> {
    pub fn get_string(&self, begin: usize, end: usize) -> &str {
        let start = std::cmp::min(begin, self.len());
        let stop = std::cmp::min(end, self.len());

        if self.buffer.is_char_boundary(start) && self.buffer.is_char_boundary(stop) {
            return &self.buffer[start..stop];
        }

        ""
    }

    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    pub fn get_char(&self, pos: usize) -> char {
        if pos >= self.len() {
            return 0 as char;
        }
        self.buffer.as_bytes()[pos] as char
    }

    pub fn find_nearest(&self, find: &str, from_index: usize) -> Option<usize> {
        self.buffer[from_index..].find(find)
    }

    pub fn new(buffer: &'a str, name: &'a str) -> Code<'a> {
        Code { buffer, name }
    }
}
