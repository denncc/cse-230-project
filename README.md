Haskell Puzzle Solver
## Overview
This Haskell Puzzle Solver is a command-line application built using the brick library. The goal of this project is to provide a versatile and extensible platform for solving various puzzles interactively through the terminal. While the initial focus will be on Sudoku, the architecture is designed to accommodate additional puzzle types in the future.

## Features
- Sudoku Solver: The application starts with a Sudoku puzzle solver, allowing users to input a puzzle and receive a solved solution.

- Interactive Interface: Utilizing the brick library, the application provides an interactive and user-friendly interface for solving puzzles. Users can navigate through cells, input values, and visualize the solution dynamically.

- ~~Extensibility: The project is designed with extensibility in mind. The codebase allows for easy integration of new puzzle types, making it a potential hub for various logic puzzles.~~

## Updates

### Architecture

- UI: Use brick library to implement TUI, which displays a sudoku and instruction of commands. Users can input a sudoku puzzle in the TUI and the solution or error message will also be displayed in the UI. The UI is responsible for collecting user input and constructing self-defined Sudoku type data.
- Solver: The solver is a function that accepts Sudoku type data and return the corresponding solution or raise exception when errors occur.

### Challenges
- We encountered difficulties while attempting to build the initial starter code for our project. Faced with this challenge, we engaged in a thorough debugging process to identify and resolve the underlying issues. This involved carefully inspecting the code and tracing the execution flow. Through persistent efforts, we successfully pinpointed the issues and implemented the necessary corrections, ultimately overcoming the obstacles in the starter code development phase.
- We encountered significant challenges with our solver when it faced unsolvable Sudoku puzzles, often resulting in infinite loops and considerable time wastage. Upon investigation, we found a major flaw in our initial code: a lack of handling for empty puzzle lists, leading to these infinite loops. To resolve this, we implemented specific checks for empty lists. Additionally, we optimized the solver's performance by refining our number insertion strategy. Instead of assessing the entire Sudoku matrix with each number placement, we pre-filtered feasible numbers for each empty space, considering the numbers already in the corresponding row, column, and 3x3 grid. This optimization significantly sped up the solver and enhanced its efficiency

### Expectation
- We expect to meet most of our goals by the deadline, except the goal of extensibility.

### Modification to goals
- Considering our familiarity with Haskell and time we have, we decide to give up the goal of extensibility.
