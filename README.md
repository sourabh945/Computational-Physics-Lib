# Computational Physics Library

## Overview

This repository contains a collection of Computational Physics programs designed to solve complex physics problems and execute physics simulations. Many directories also include practical file documentations (PDFs) detailing the theory and flowcharts behind the implementations.

## Technical Details

* **Primary Language:** The programs are primarily written in Fortran 95, with some tools utilizing Python for translations into Fortran.
* **Visualization:** All graphical plots and phase diagrams are generated using Gnuplot.
* **Platform Support:** The pre-compiled executables included in the repository are built for Linux environments.

## Included Numerical Methods & Simulations

The library is categorized into various fundamental computational methods and physics applications:

**Differential Equations & Oscillators:**

* Damped Driven Pendulum (Includes phase diagram generation)
* Harmonic Oscillator (Calculations for single and multi-values)
* Runge Kutta Method

**Numerical Integration:**

* Gauss Quadrature Method (1st and 2nd degree)
* Simpson 1/3 Rule (1st and 2nd degree)

**Interpolation & Curve Fitting:**

* Lagrange Interpolation
* Newton Forward Interpolation (Continuous and single-value)
* Least Square Fitting

**Linear Algebra & Matrix Operations:**

* Gauss Elimination Method
* Gauss Jordan Method
* Pivot Condensation (Solves for Matrix Determinant and Matrix Inverse)
* Power Method

**Monte Carlo Simulations:**

* Estimation of the Area of a Circle
* Simulation of Brownian Motion
* Simulation of Radioactive Decay

**Electronics & Circuit Analysis:**

* LCR Frequency Response

## Branch Structure & Status

* **Main Branch:** All programs located in the main branch are thoroughly tested and verified to fit all standard cases.
* **Test Branch:** Any new or experimental programs currently under development are hosted in the test branch.

## Feedback & Issues

Please report any issues or errors you encounter while running the programs.
