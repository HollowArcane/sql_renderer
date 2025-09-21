import toolkit_util/lists
import sql_renderer/translator
import glance
import simplifile
import gleam/io
import sql_renderer/renderer

pub fn main() -> Nil {
    // Here is how it should look like
    // gleam file -> tokenizer(glance) -> treeifier?(glance) -> translator -> make sure the code is coherentinator -> tokenizer2 -> formatter -> sql file
    let assert Ok(_) = transpile("src/test_module.gleam")
    io.println("Hello from sql_renderer!")
}

pub fn transpile(filename: String)
{
    // MAKE BETTER ERROR HANDLING
    let assert Ok(content) = simplifile.read(filename)
    let assert Ok(module) = glance.module(content)
    let assert Ok(sql_module) = translator.translate(module)
    let render = renderer.render(sql_module)
    simplifile.write(render, to: filename <> ".sql")
}
