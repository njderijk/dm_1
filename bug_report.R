conf_matrix <- table(true = true_values$true, pred = y)
print(conf_matrix)

# The accuracy is equal to the diagonal
# The recall is the sensitivity = TPR
# The precision is the PPV

## CONFUSION MATRIX
#0 = Negative
#1 = Postive

TN <- conf_matrix[1,1] 
TP <- conf_matrix[2,2] 
FN <- conf_matrix[2,1] 
FP <- conf_matrix[1,2]

tibble(TPR = TP / (TP+FN), #sensitivity 
       TNR = TN / (TN+FP), #specificity 
       FPR = FP / (FP+TN),
       FNR = FN / (FN+TP),
       PPV = TP / (TP + FP), # Precision 
       NPV = TN / (TN + FN),
       Acc = (TP + TN) / sum(conf_matrix))