# Part C - Python Integration Layer Explanation

## Multi-Paradigm Programming in Python

This Python integration layer demonstrates multi-paradigm programming by seamlessly combining **Object-Oriented Programming (OOP)** and **Functional Programming** paradigms within a unified system architecture.

### Object-Oriented Programming (OOP)

Python supports OOP through class-based modeling. The `Student` class encapsulates student data (id, name, program, grades) and behaviors (predict_performance method), demonstrating encapsulation and abstraction. The `UniversitySystem` class manages system operations, showing how OOP organizes code into logical, reusable components with clear responsibilities.

### Functional Programming

Python supports functional programming through higher-order functions and lambda expressions. The system uses `map()` to generate performance predictions for all students, `filter()` with lambda functions to identify distinction students and at-risk students, and `sorted()` with lambda keys to rank students by average. These operations demonstrate pure function composition, immutability principles, and declarative data transformations without side effects.

### System Integration

Python acts as the integration layer in a layered architecture, orchestrating external engines using `subprocess`. It calls the Haskell functional computation engine (`runhaskell grades.hs`) for grade analysis and the Prolog logic engine (`swipl advisory.pl`) for rule-based reasoning. Python merges these results, demonstrating its role as a multi-paradigm glue language that bridges different programming paradigms and external systems.

### Data Presentation

Using pandas, the system creates structured DataFrames for data visualization, showcasing Python's ecosystem integration. The console interface provides an interactive menu system, demonstrating practical application of multi-paradigm design in real-world software systems.
