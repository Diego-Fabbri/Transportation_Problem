# Transportation Problem

A **Linear Programming (LP)** model in **R** for the classic **Transportation Problem**, built with the [`ompr`](https://dirkschumacher.github.io/ompr/) modeling framework and solved via the **SYMPHONY** solver (through `ROI`). Instance data is provided alongside the script in an **Excel workbook**.

## Overview

The Transportation Problem is a fundamental network flow optimization problem in Operations Research. Goods are produced at $m$ supply centers and must be shipped to $n$ demand centers to satisfy known demand — at minimum total shipping cost.

Each supply center has a maximum capacity it cannot exceed, and each demand center must receive exactly its required quantity. Before solving, the model checks a **feasibility condition**: total supply must be at least equal to total demand. If this condition holds, the problem is guaranteed to have a feasible solution.

## Repository Contents

| File | Description |
|---|---|
| `Transportation Problem.R` | R script implementing and solving the Transportation Problem instance |
| `Transportation_Problem.xlsx` | Excel workbook containing the problem instance data (cost matrix, supply, demand) |
| `Transportation Problem_Math Formulation.pdf` | Mathematical formulation of the problem |

## Mathematical Formulation

### Sets and Parameters

- $m$ = number of supply centers (index $i = 1, \dots, m$)
- $n$ = number of demand centers (index $j = 1, \dots, n$)
- $S_i$ = supply available at supply center $i$
- $d_j$ = demand required at demand center $j$
- $c_{ij}$ = unit shipping cost from supply center $i$ to demand center $j$

### Feasibility Condition

The problem has a feasible solution only if total supply is sufficient to cover total demand:

$$
\displaystyle \sum_{j=1}^{n} d_j \le \sum_{i=1}^{m} S_i
$$

### Variable

- $x_{ij}$ = number of units shipped from supply center $i$ to demand center $j$; $x_{ij} \ge 0,\ x_{ij} \in \mathbb{Z}$

### Objective Function

**(1)** — Minimize total shipping cost

$$
\displaystyle \min \sum_{i=1}^{m} \sum_{j=1}^{n} c_{ij} \cdot x_{ij}
$$

### Constraints

**(2)** — Supply capacity: shipments from each supply center cannot exceed its available supply

$$
\displaystyle \sum_{j=1}^{n} x_{ij} \le S_i \qquad \forall\, i = 1, \dots, m
$$

**(3)** — Demand satisfaction: each demand center must receive exactly its required quantity

$$
\displaystyle \sum_{i=1}^{m} x_{ij} = d_j \qquad \forall\, j = 1, \dots, n
$$

**(4)** — Non-negative integer shipments

$$
x_{ij} \ge 0, \quad x_{ij} \in \mathbb{Z} \qquad \forall\, i = 1, \dots, m,\ j = 1, \dots, n
$$

> **Note on integrality:** The transportation problem with integer supply and demand values always admits an integer optimal solution (the constraint matrix is totally unimodular). Variables are therefore modeled as integers — appropriate when goods are discrete units. The constraint in (2) is an inequality ($\le$) rather than equality, allowing supply centers not to ship all available goods when supply exceeds demand.

A copy of this formulation is also available as a standalone PDF in this repository.

## Example Instance

The script uses the following hardcoded instance with **3 supply centers** and **4 demand centers**:

**Supply capacities:**

| Supply center | Capacity $S_i$ |
|:---:|---:|
| $S_1$ | 150 |
| $S_2$ | 20 |
| $S_3$ | 130 |
| **Total** | **300** |

**Demand requirements:**

| Demand center | Demand $d_j$ |
|:---:|---:|
| $D_1$ | 135 |
| $D_2$ | 75 |
| $D_3$ | 45 |
| $D_4$ | 45 |
| **Total** | **300** |

**Unit shipping cost matrix $C$:**

$$
C = \begin{pmatrix}
5 & 2 & 3 & 9 \\
7 & 1 & 12 & 4 \\
8 & 15 & 19 & 2
\end{pmatrix}
$$

This is a **balanced** instance: total supply (300) equals total demand (300), so the feasibility condition holds and all supply is fully allocated. The instance data is also available in the included Excel workbook `Transportation_Problem.xlsx`.

## Requirements

```r
install.packages(c("lpSolve", "dplyr", "ROI", "ROI.plugin.symphony", "ompr", "ompr.roi"))
```

## Usage

1. Clone or download this repository.
2. Open `Transportation Problem.R` in R or RStudio.
3. Update the `setwd()` path at the top of the script to match your local directory.
4. Run the script. It will:
   - Check the feasibility condition (total supply ≥ total demand)
   - If feasible, build and solve the model using `ompr` and SYMPHONY
   - Print the solver status, optimal total shipping cost, and the active shipment flows $x[i,j] > 0$ with their individual costs

## Output

The script prints:

- **Feasibility check** — whether the supply is sufficient to cover total demand
- **Model status** — whether an optimal solution was found
- **Objective value** — the minimum total shipping cost
- **Active shipments** — for each supply center, all demand centers it ships to, the quantity shipped, and the associated shipping cost
