
# Royal Roads for When Crossover s Essential #

This genetic algorithm is modelled after the one described in Thomas Jansen and Ingo Wegener's paper "Real royal road functions - where crossover provably is essential".

## Running ##

Requirements: SBCL (and for the GUI also quicklisp must be installed)

The two parameters taken are chromosome size and block size within the chromosome. In the paper these ar the two index parameters of their function R and are denoted as 'n' and 'm'.

    $ sbcl --script main.lisp 32 4

The result value will be the run cycles it took to find the solution.
