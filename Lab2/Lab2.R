
rm(list=ls())
prob_roll_loaded = c(rep(0.1,5), 0.5)
prob_roll_fair = c(rep(0.1,6))

prob_dice <-c(0.01, 0.99)

post_prob <-vector()

data<-c(2,3,2,6,3,5,6,2,6,6,2,6,6,2,3,6,6,6,5,6,6,5,6,6,6,6,6,4,6,3,3,3,6,6,5,6,6)

# (1.1) Graph of the posterior probability as a function of the number of times having
# rolled the dice

for( k in 1:length(data)){
  
  post_prob[k] <- prob_dice[1]
  denom <- prob_dice[1]*prob_roll_loaded[data[k]] + prob_dice[2]*prob_roll_fair[data[k]]
  
  prob_dice[1] = prob_dice[1]*prob_roll_loaded[data[k]]/denom
  prob_dice[2] = prob_dice[2]*prob_roll_fair[data[k]]/denom
}
plot(1:k,post_prob)

# (1.2) How many times on average would you need to roll a loaded die to be 
# 99.999% sure that it was loaded
j = 0
num_rolls <- vector()
while(j < 100){
  j = j + 1
  prob_dice <-c(0.01, 0.99)
  loaded_prob <-vector()
  i = 1
  while(prob_dice[1] < 0.99999){
    loaded_prob[i] <- prob_dice[1]
    i = i + 1
    x <- sample(1:6,1,replace = FALSE, prob = prob_roll_loaded)
    denom <- prob_dice[1]*prob_roll_loaded[x] + prob_dice[2]*prob_roll_fair[x]
    prob_dice[1] = prob_dice[1]*prob_roll_loaded[x]/denom
    prob_dice[2] = prob_dice[2]*prob_roll_fair[x]/denom
  }
  num_rolls[j] = i
}
# Average number of rolls needed to be 99.999% sure that a die is loaded
mean(num_rolls)

hospital_requirement = 0.99999
prob_disease <- c(0.001,0.999)
likelihoodGivenDisease <- c(0.91,0.09)
likelihoodGivenNoDisease <- c(0.16,0.84)
# (2.1) Run simulations for a patient with the disease. About how many time on average 
# must the test be repeated to achieve the hospitals requirements

# 1 for positive, 2 for negative
total_tests <-vector()
i = 0
while(i<1000){
    prob_disease <- c(0.001,0.999)
    i = i + 1
    test_count = 0
    while(prob_disease[1] < hospital_requirement){
      x<- sample(1:2,1,prob=likelihoodGivenDisease)
      test_count = test_count + 1
        prob_positive = prob_disease[1]*likelihoodGivenDisease[x] + prob_disease[2]*likelihoodGivenNoDisease[x]
        
        prob_disease[1] = likelihoodGivenDisease[x]*prob_disease[1]/prob_positive
        prob_disease[2] = likelihoodGivenNoDisease[x]*prob_disease[2]/prob_positive
    }
    total_tests[i] = test_count
}
avg_num_tests_disease = mean(total_tests)
print(avg_num_tests_disease)

# (2.2) Repeat 2.1 but the patient doesn't have the disease
total_tests <-vector()
i = 0
while(i<1000){
    prob_disease <- c(0.001,0.999)
    i = i + 1
    test_count = 0
    while(prob_disease[2] < hospital_requirement){
      test_count = test_count + 1
      x<- sample(1:2,1,prob=likelihoodGivenNoDisease)
      prob_positive = prob_disease[1]*likelihoodGivenDisease[x] + prob_disease[2]*likelihoodGivenNoDisease[x]
      
      prob_disease[1] = likelihoodGivenDisease[x]*prob_disease[1]/prob_positive
      prob_disease[2] = likelihoodGivenNoDisease[x]*prob_disease[2]/prob_positive
    }
    total_tests[i] = test_count
}
avg_num_tests_no_disease = mean(total_tests)
print(avg_num_tests_no_disease)

# (2.3) 

total_people = 100000
total_people_disease = prob_disease[1] * total_people
total_people_no_disease = prob_disease[2] * total_people

number_of_tests_total = total_people_no_disease*avg_num_tests_no_disease 
            + total_people_disease*avg_num_tests_disease

