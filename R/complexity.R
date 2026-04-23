


##### A Predictive Coding Approach to Modelling Perceived Drum Pattern Complexity
#####
##### Script to estimate drum pattern complexity
#####
##### Created using R version 4.1.2 (2021-11-01); RStudio  version 2022.07.1+554
#####
##### Olivier Senn (January 10, 2023)


##### Load resources #####

### Double-click on the following files in order to open them in R/RStudio:

# "M0.Rda" : Opens 65x3 matrix M0 that encodes the modes (probabilities) of the prior model, as in Table 4:
# View(M0)

# "S.Rda"  : Opens 65x3x40 array S that encodes the i=40 drum patterns with l=3 instrumental layers and k=65 discrete times
#            in binary form. See the pattern of stimulus 13 ("Bravado) as in Table 3:
# View(S[,,13]) # Other patterns can be accessed by replacing 13 with the appropriate stimulus number

# "U.Rda"  : Opens 65x65 matrix U that projects expectations forward


##### Create the 65x3 all-ones matrix J #####
# J <- matrix(1, nrow = 65, ncol = 3)


##### Predict complexity function #####

predict_complexity <- function (Si, M0, lambda = 3.56, C = 2.492082, D = -6.926637){# Si = drum pattern, M0 = modes of prior distribution
  # Create 65x3 all-ones matrix J
  J <- matrix(1, nrow = 65, ncol = 3)
  # Calculate A0 (65x3 matrix)
  A0 <- (lambda - 2)*M0 + J
  # Calculate B0 (65x3 matrix)
  B0 <- lambda*J - (lambda - 2)*M0 - J
  # Calculate Ai (65x3 matrix)
  Ai <- U %*% Si + A0
  # Calculate Bi (65x3 matrix)
  Bi <- U %*% (J - Si) + B0
  # Calculate Mi (65x3 matrix)
  Mi <- (Ai - 1) / (Ai + Bi - 2)
  # Calculate sigma_i (65x3 matrix, the surprisal function)
  sigma_i <- -log(abs(Si + Mi - J))
  # Calculate Ei (the effort Ei associated with stimulus Si)
  Ei <- sum(sigma_i)
  # Calculate gamma_i (the predicted complexity of stimulus Si)
  gamma_i <- C*log(Ei) + D
  # Return gamma_i
  gamma_i
}


##### Use the predict_complexity function #####

### Predict the complexity of stimulus 13 ("Bravado")
# predict_complexity(Si = S[,,13], M0)
# Predict the complexity of any of the 40 test stimuli by replacing "13" by the appropriate stimulus number

### Predict the complexity of a new drum pattern

# Enter the new pattern in the appropriate format (65x3 matrix).
# The following example is the generic backbeat:
# Bass drum
# BD <- c(1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1)
# Snare drum
# SD <- c(0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0)
# Cymbals
# CY <- c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)
# Combine and view
# backbeat <- as.matrix(data.frame(BD,SD,CY))
# View(backbeat)
# Predict the complexity of the backbeat
# predict_complexity(Si = backbeat, M0)



