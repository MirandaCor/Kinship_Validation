1. Introduction
Genotype simulation is essential for forensic and kinship analysis, where allele frequencies are used to generate plausible genetic profiles.
The process involves simulating genetic data under two competing hypotheses:

H1 (Related Hypothesis): The individuals share a specified biological relationship (e.g., parent-child, siblings).
H0 (Unrelated Hypothesis): The individuals are unrelated and drawn from the general population.

In forensic genetics, this simulation helps evaluate the likelihood ratio (LR) and assess the reliability of kinship inference methods, such as those implemented in quick search:
blindsearch in Familias and ScreenMatch in DNAview, and other kinship analysis software.

2. Simulation Process
Step 1: Define the Marker Set
The analysis considers two scenarios:

15 STR markers: Standard forensic panel (e.g., IdentiFiler, ESS markers).
21 STR markers: Extended panel including additional informative loci.

Step 2: Introduce Allele Frequencies
Allele frequencies are sourced from a reference population database. These frequencies are critical for randomly generating genotypes based on population genetics principles.
Import frecuencies on General DNA Data botton in Familias in correct format (.txt). 
If allele frequencies are stored in an .xlsx table with a general format, they need to be rearranged before use. 
The R script freq_to_familias.R automates this transformation.
Example 1036-Revised-Allele-Freqs-PopStats-July-19-2017.xlsx download in NIST webpage.  

Step 3: Generate Genotypes
For each marker, genotypes are simulated under both hypotheses (H1 and H0) using the Pedigrees module. Navigate to the Simulation section, where you can configure the 
parameters according to your specific requirements. Adjust settings such as allele frequencies, number of simulations, and inheritance models

If simulating parent-child pairs: The child inherits one allele from each parent.
If simulating full siblings: Each sibling inherits alleles based on Mendelian segregation.
This process is repeated for each of the 15 or 21 markers.

Step 4: Use the SimulationRawData.txt file as input in Rearrangement_output.R to restructure the simulated profiles, organizing them by rows for further analysis.
