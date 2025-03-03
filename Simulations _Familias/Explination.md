# 1. Introduction  

Genotype simulation is essential for forensic and kinship analysis, where allele frequencies are used to generate plausible genetic profiles.  
The process involves simulating genetic data under two competing hypotheses:  

- **H1 (Related Hypothesis):** The individuals share a specified biological relationship (e.g., parent-child, siblings).  
- **H0 (Unrelated Hypothesis):** The individuals are unrelated and drawn from the general population.  

In forensic genetics, this simulation helps evaluate the **likelihood ratio (LR)** and assess the reliability of kinship inference methods, such as those implemented in **quick search tools**:  
- **BlindSearch** in **Familias**  
- **ScreenMatch** in **DNAview**  
- Other kinship analysis software  

---

# 2. Simulation Process  

## **Step 1: Define the Marker Set**  

The analysis considers two scenarios:  

- **15 STR markers:** Standard forensic panel (e.g., IdentiFiler, ESS markers).  
- **21 STR markers:** Extended panel including additional informative loci.  

## **Step 2: Introduce Allele Frequencies**  

Allele frequencies are sourced from a **reference population database**. These frequencies are critical for randomly generating genotypes based on population genetics principles.  

- **Import frequencies** in **Familias** via the **General DNA Data** button using the correct format (`.txt`).  
- If allele frequencies are stored in an `.xlsx` table with a general format, they need to be **rearranged** before use.  
- The R script `freq_to_familias.R` automates this transformation.  

**Example:**  
A frequency file such as **`1036-Revised-Allele-Freqs-PopStats-July-19-2017.xlsx`** can be downloaded from the **NIST webpage**.  

## **Step 3: Generate Genotypes**  

For each marker, genotypes are simulated under both hypotheses (**H1 and H0**) using the **Pedigrees** module.  

1. Navigate to the **Simulation** section in **Familias**.  
2. Configure the **parameters** according to your specific requirements.  
3. Adjust settings such as **allele frequencies, number of simulations, and inheritance models**.  

### **Inheritance Models:**  
- **Parent-child pairs:** The child inherits one allele from each parent.  
- **Full siblings:** Each sibling inherits alleles based on **Mendelian segregation**.  

This process is repeated for each of the **15 or 21 markers**.  

## **Step 4: Prepare Simulated Profiles**  

- Use the **`SimulationRawData.txt`** file as input in `Rearrangement_output.R`.  
- This script restructures the simulated profiles, organizing them by rows for further analysis.  
