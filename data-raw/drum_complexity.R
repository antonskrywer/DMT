

##### Use the predict_complexity function #####

### Predict the complexity of stimulus 13 ("Bravado")
# predict_complexity(Si = S[,,13], M0)
# Predict the complexity of any of the 40 test stimuli by replacing "13" by the appropriate stimulus number

### Predict the complexity of a new drum pattern

# Enter the new pattern in the appropriate format (65x3 matrix).
# The following example is the generic backbeat:
# Bass drum

BD <- c(1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1)

# Snare drum
SD <- c(0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0)

# Cymbals
CY <- c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)

# Combine and view

backbeat <- as.matrix(data.frame(BD,SD,CY))

View(backbeat)

# Predict the complexity of the backbeat
predict_complexity(Si = backbeat, M0)

