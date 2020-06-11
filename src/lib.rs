
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

use runner::runner::*;

mod core;
mod parser;
mod runner;

#[wasm_bindgen]
pub struct WasmInterface {
    runner: Runner
}

#[wasm_bindgen]
pub struct IFaceResult {
    wrapped: Result<String, String>
}

impl IFaceResult {
    pub fn new(wrapped: Result<String, String>) -> IFaceResult {
        IFaceResult {wrapped}
    }
}

#[wasm_bindgen]
impl IFaceResult {
    pub fn is_ok(&self) -> bool {
        self.wrapped.is_ok()
    }

    pub fn get_decl(&self) -> String {
        match &self.wrapped {
            Ok(s) => s.clone(),
            Err(s) => s.clone()
        }
    }
}

#[wasm_bindgen]
impl WasmInterface {
    pub fn new() -> WasmInterface {
        WasmInterface {runner: Runner::new()}
    }

    pub fn register_axioms(&mut self, axioms_def: &str) -> IFaceResult {
        IFaceResult::new(match self.runner.register_axioms(axioms_def) {
            Ok(()) => Ok("Ok".to_string()),
            Err(e) => Err(format!("{}", e))
        })
    }

    pub fn exec_proof(
        &mut self, decl: &str
    ) -> IFaceResult {
        IFaceResult::new(match self.runner.exec_proof(decl) {
            Ok(res) => Ok(res),
            Err(e) => {
                let mut buf = vec!();
                e.pretty_display(&mut buf, decl).unwrap();
                Err(String::from_utf8(buf).unwrap())
            }
        })
    }
}
