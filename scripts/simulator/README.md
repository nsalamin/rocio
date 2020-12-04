# Nicolas' R simulator with some improvement

## Example script

See example script for usage.

## Modifications
* [WARNING] Average branch lengths are now specified to define the tree length. See in the example script how to specify the average branch length (*meanBL*) to obtain an average tree length of X.
* Added +Gamma rate: parameter *gammaRate*. If *gammaRate* == NULL or *gammaRate* is high (e.g., >>1), there is no or few rate heterogeneity among site. If *gammaRate* is low (e.g., << 1), then there is a high rate heterogeneity among site.
* Added an option for uniform frequencies: parameter *uniformFreq*. If *uniformFreq* is set to **FALSE**, each state's entry in the Q matrix is weighted by a factor drawn from a Dirichlet distributions (*Nicolas' original implementation*). If *uniformFreq* is set to **TRUE**, then no weights are applied. In this case, the stationary frequency of each pair of states in the profile can be estimated using the *compute_coev_pair_stationaryFreq*. The opposite computations (i.e., which d/s for a given pair-in-profile stationary frequencies) can be estimated with *compute_coev_pair_dsRatio*.
* Added an option to draw the tree and the substitutions simulated for pairs simulated as coevolving: parameter *figFolder*. This parameter provide the folder in which the **pdf** drawing will be saved.
* Added the field *substCountCoev* in return the return of a Coev simulation. This variable is a list containing the number of substitution observed during the simulation of a coevolving pair.
