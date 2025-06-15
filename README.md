# create-logic-hs

# Haskell Tableau SAT Checker

A simple REPL tool for checking the **satisfiability of propositional formulas** using the **semantic tableau method**, written in Haskell.

🧠 **This project is inspired by the tableau method introduced in _『論理学をつくる』_ by Kazuhisa Todayama (戸田山和久).**  

📚 Reference:  
『論理学をつくる』戸田山 和久 著（名古屋大学出版会）  
<https://www.unp.or.jp/ISBN/ISBN4-8158-0390-0.html>

## ✨ Features

- Uses tableau (truth tree) method to check satisfiability.
- Interactive REPL interface.
- Custom syntax for writing propositional formulas.
- Immediate feedback on whether the given formulas are satisfiable.

## 🔤 Syntax

Use the following symbols to write formulas:

| Operator | Symbol | Example     |
|----------|--------|-------------|
| NOT      | `~`    | `~P`        |
| AND      | `&`    | `P & Q`     |
| OR       | `|`    | `P | Q`     |
| IMPLIES  | `->`   | `P -> Q`    |
| Separator between formulas | `;` | `P -> Q; P; ~Q` |

- Parentheses `()` can be used for grouping.
- Each proposition is represented by an uppercase identifier like `P`, `Q`, etc.
- Use `;` to separate multiple formulas that are assumed to be true together.

## 🧪 Example

To check whether the set of formulas `{P ∧ Q, P}` is satisfiable, you can input:
```
P & Q; P
```

If the set is satisfiable, you’ll see:
```
result: True
```

Otherwise, for an unsatisfiable set like `P; ~P`, you’ll get:
```
result: False
```

## 🖥️ How to Use

### 🐳 Option 1: Run with Docker (Recommended)

Make sure you have [Docker](https://www.docker.com/) and `make` installed.

#### 🔧 Build the Docker image

```bash
make build
```
#### Run the REPL inside a Docker container

```bash
make run
```

This will launch the REPL in your terminal. Example session:

```
> P -> Q; P
Satisfiable

> P; ~P
Unsatisfiable

> :q
Bye!
```

### 💻 Option 2: Run with Stack (for Haskell developers)

If you prefer running it directly with Stack:
1.	Clone the repository:
```bash
git clone https://github.com/f-ttanaka/create-logic-hs.git
cd create-logic-hs
```

2.	Build the project:
```bash
stack build
```

3.	Run the REPL:
```bash
stack run
```
