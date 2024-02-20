pub struct DeclarationCounter<T = ()> {
    counter: std::collections::HashMap<String, Vec<T>>,
    blocks: Vec<Vec<String>>,
}

impl<T> Default for DeclarationCounter<T> {
    fn default() -> Self {
        Self {
            counter: std::collections::HashMap::new(),
            blocks: vec![],
        }
    }
}

impl<T> DeclarationCounter<T> {
    pub fn add_with(&mut self, name: &str, value: T) {
        if let Some(target) = self.counter.get_mut(name) {
            target.push(value);
        } else {
            self.counter.insert(name.to_owned(), vec![value]);
        }

        if let Some(block) = self.blocks.last_mut() {
            block.push(name.to_owned());
        }
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.counter.get(name)?.last()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.counter.contains_key(name)
    }

    pub fn start_block(&mut self) {
        self.blocks.push(vec![]);
    }

    pub fn end_block(&mut self) {
        for name in self.blocks.pop().unwrap() {
            self.subtract(&name);
        }
    }

    fn subtract(&mut self, name: &str) {
        let value = self
            .counter
            .get_mut(name)
            .expect("Tried to remove undeclared identifier");

        value.pop();

        if value.is_empty() {
            self.counter.remove(name);
        }
    }
}

impl DeclarationCounter<()> {
    pub fn add(&mut self, name: &str) {
        self.add_with(name, ());
    }
}
