#[derive(Default)]
pub struct DeclarationCounter<T = ()> {
    counter: std::collections::HashMap<String, Vec<T>>,
}

impl<T> DeclarationCounter<T> {
    pub fn add_with(&mut self, name: &str, value: T) {
        if let Some(target) = self.counter.get_mut(name) {
            target.push(value);
        } else {
            self.counter.insert(name.to_owned(), vec![value]);
        }
    }

    pub fn subtract(&mut self, name: &str) {
        let value = self
            .counter
            .get_mut(name)
            .expect("Tried to remove undeclared identifier");

        if value.pop().is_none() {
            self.counter.remove(name);
        }
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.counter.get(name)?.last()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.counter.contains_key(name)
    }
}

impl DeclarationCounter<()> {
    pub fn add(&mut self, name: &str) {
        self.add_with(name, ());
    }
}
