set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98, 0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90, 0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#Question 2_______________________________________________________________________________
test_positive <- mean(test)
test_positive

#Question 3_______________________________________________________________________________
test_negative <- 1 - test_positive
disease_given_test_negative <- mean(test[disease==1] == 0) * mean(disease == 1) / test_negative
disease_given_test_negative

#Question 4_______________________________________________________________________________
disease_given_test_positive <- mean(test[disease==1] == 1) * mean(disease == 1) / test_positive
disease_given_test_positive

#Question 5_______________________________________________________________________________
disease_given_test_positive / mean(disease == 1)


