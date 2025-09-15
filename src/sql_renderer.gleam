import gleam/io

pub type Table
{ Table(name: String, columns: List(Column)) }

pub type Column
{ Column(name: String, type_: Type) }

pub type Type
{
	Serial
	Varchar(Int)
	Numeric(digits: Int, decimal: Int)
	Date
	Time
	Timestamp
}

pub fn create_table()
{}

pub fn main() -> Nil {
  io.println("Hello from sql_renderer!")
}
