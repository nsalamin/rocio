# Nicolas' R simulator with some improvement

## Example script

See example script for usage.

## Independent model of substitutions (NEW : 11/01/2021)

You can chose between two models of substitutions for sites evolving independently.

Choosing a model of substitution for independent sites:
> x<-simulateCoev(s=1, d=100, r=5, nsp=100, nCoevol=10, nNonCoev=80, meanBL=meanBL[iBL], figFolder=figFolder, gammaRate = 2., indepModel="CAT")

The last parameter **indepModel** can take two values:
* **"LG"**
  * original setting in Nicolas' script
* **"CAT"**
  * new setting


### **LG** model

The original **LG** model ("LG"). The A.A. here evolves *freely*: that is, the evolution along branches is unconstrained.

### **CAT** model

The **CAT** model (*Lartillot and Philippe, 2004*) assumes several **CAT**egories of independent sites that evolve under different profiles. Each site (but not pair of sites, contrary to **Coev**) evolves under a specific profile. Inference with the full model will estimates
* The number of profiles (or categories)
* The type of profiles (e.g., A<->L<->W)
* Their frequency in the mixture of profiles
* The transition rate of their rate matrix Q

In the simulator, the CAT model used has 20 categories estimated by *Le, Lartillot and Gascuel, 2008* study (a 50-categories CAT model is also available in the simulator). In this study, the parameters of CAT models with fixed number of profiles (e.g., 20, 50) were estimated on a large dataset of A.A. alignments. These CAT models were tested against WAG model (e.g., simpler "LG"-like model) and were strongly supported.

## Modifications (4/12/2020)
* [WARNING] Average branch lengths are now specified to define the tree length. See in the example script how to specify the average branch length (*meanBL*) to obtain an average tree length of X.
* Added +Gamma rate: parameter *gammaRate*. If *gammaRate* == NULL or *gammaRate* is high (e.g., >>1), there is no or few rate heterogeneity among site. If *gammaRate* is low (e.g., << 1), then there is a high rate heterogeneity among site.
* Added an option for uniform frequencies: parameter *uniformFreq*. If *uniformFreq* is set to **FALSE**, each state's entry in the Q matrix is weighted by a factor drawn from a Dirichlet distributions (*Nicolas' original implementation*). If *uniformFreq* is set to **TRUE**, then no weights are applied. In this case, the stationary frequency of each pair of states in the profile can be estimated using the *compute_coev_pair_stationaryFreq*. The opposite computations (i.e., which d/s for a given pair-in-profile stationary frequencies) can be estimated with *compute_coev_pair_dsRatio*.
* Added an option to draw the tree and the substitutions simulated for pairs simulated as coevolving: parameter *figFolder*. This parameter provide the folder in which the **pdf** drawing will be saved.
* Added the field *substCountCoev* in return the return of a Coev simulation. This variable is a list containing the number of substitution observed during the simulation of a coevolving pair.


## References
* Lartillot, Nicolas, and Hervé Philippe. “A Bayesian Mixture Model for Across-Site Heterogeneities in the Amino-Acid Replacement Process.” Molecular Biology and Evolution 21, no. 6 (June 1, 2004): 1095–1109. https://doi.org/10.1093/molbev/msh112.
* Le, Si Quang, Nicolas Lartillot, and Olivier Gascuel. “Phylogenetic Mixture Models for Proteins.” Philosophical Transactions of the Royal Society B: Biological Sciences 363, no. 1512 (December 27, 2008): 3965–76. https://doi.org/10.1098/rstb.2008.0180.
