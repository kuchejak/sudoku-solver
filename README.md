# Sudoku Solver

This project is a Sudoku solver implemented in Racket. It was created as a homework assignment for the Programming Paradigms course at the Czech Technical University in Prague. It was created at the Czech Technical University in Prague as a homework for the "Programming Paradigms" course.

## Overview

The Sudoku solver can handle various difficulties of Sudoku puzzles and solve any valid (solvable) sudoku, ranging from easy to extra hard, even for larger or smaller grids (like 16x16). It includes functions for printing the Sudoku puzzle, validating the input, and solving the puzzle using a brute-force algorithm.

## Sudoku Samples

The project includes several predefined Sudoku puzzles of varying difficulties:

- `sudoku1`: Easy
- `sudoku2`: Harder
- `sudoku3`: Extra Hard
- `sudoku4`: Single cell (edge case test)
- `sudoku5`: 4x4 Sudoku
- `sudoku6`: 16x16 Sudoku

## Usage

### Printing Sudoku

The function `sud-print` can be used to print a Sudoku puzzle in a readable format.

### Validating Sudoku

The function `validate-sudoku` checks whether the Sudoku puzzle is valid.

### Solving Sudoku

The function `solve-sudoku` is the main function to solve the given Sudoku puzzle. It first validates the puzzle and then uses a brute-force algorithm to find the solution.

### Example

Here is an example of how to use the solver:

```racket
#lang racket
(require "sudoku-solver.rkt")

(define sudoku
  '((0 0 0 2 6 0 7 0 1)
    (6 8 0 0 7 0 0 9 0)
    (1 9 0 0 0 4 5 0 0)
    (8 2 0 1 0 0 0 4 0)
    (0 0 4 6 0 2 9 0 0)
    (0 5 0 0 0 3 0 2 8)
    (0 0 9 3 0 0 0 7 4)
    (0 4 0 0 5 0 0 3 6)
    (7 0 3 0 1 8 0 0 0)))

(solve-sudoku sudoku)
```

## Files

- `sudoku-solver.rkt`: The main Racket file containing all the functions for solving Sudoku.
- `README.md`: This README file.


# sudoku-solver
