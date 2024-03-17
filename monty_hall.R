
#### The Monty Hall Problem ####
### Code written by Stephen M. Baum ###
### Last updated on 03.17.2024 ###

# Note - the assumptions and conditions that I am using #
# to define this problem space can be found here: #
# https://sites.oxy.edu/lengyel/M372/Vazsonyi2003/vazs30_1.pdf #
# I am assuming that the prize is one million in cash, not a car #

#### Method I - States of the World ####

# there are three states of the world
state_one = list(D1 = "cash", D2 = "goats", D3 = "goats")
state_two = list(D1 = "goats", D2 = "cash", D3 = "goats")
state_three = list(D1 = "goats", D2 = "goats", D3 = "cash")

# in each state, we will assume D1 is chosen by contestant
contestant_one = state_one$D1 # cash
contestant_two = state_two$D1 # goats
contestant_three = state_three$D1 # goats

# then, Monty opens a door

# crucially, he will always open a door that:
# (1) is not the door you selected,
# (2) is not the money door

# in each state, what does Monty choose?
monty_one = state_one$D2 # could be D3, they are substitutes
monty_two = state_two$D3
monty_three = state_three$D2

# so, what is left if you switch?
switch_one = state_one$D3
switch_two = state_two$D2
switch_three = state_three$D3

# in summary
contestant_choices = c(contestant_one,contestant_two,contestant_three)
monty_reveals = c(monty_one,monty_two,monty_three)
switch_outcome = c(switch_one,switch_two,switch_three) 

# the outcomes
contestant_choices # 1/3 of staying cases result in cash
switch_outcome # 2/3 of switching cases result in cash

#### Method II - Bayesian Logic ####

# much of the logic here comes from Causal Inference: The Mixtape
# credit to Scott Cunningham

# we will call the probability that the money is behind door i to be Ai 
# we will call the doors 'D' (i.e., D1 is door 1, etc.)
# we will use 'pr' to describe the probability of some event (i.e., pr(A))
# conditional probabilities are weird 
# we will NOT write pr(A|B) - R appears to have trouble with that 
# we will write pr(a_b), or a_b instead 

## Let's assume that you select D1 ##

# we have a totally unconditional prior
pr_a1 = 1/3 # 1/3 chance we are right
pr_not_a1 = 2/3 # 2/3 chance we are wrong

## Monty opens D2! ##

# there is a goat behind D2 

# we are interested in the probability that we choose correctly
# we can now incorporate new information into our calculation
# Monty opened D2 (Event B) to reveal goats

# thus, we are interested in Pr(A1|B) 
# given that B happened, what is the probability the money is behind door A1?

# write a function for Pr(A1|B)
pra1_bfunct = function(b_a1, a1, b_a2, a2, b_a3, a3) { # NOTE - these technically should be wrapped by Pr() #
  res = (b_a1*a1) / ((b_a1*a1) + (b_a2*a2) + (b_a3*a3)) 
  return(res)
}

# defining all of the probabilistic components 

# conditional probabilities

# the key intuition to remember is that all of these take into account to what you've chose - D1

b_a1 = 1/2 # the probability that you choose correctly (money is behind D1), and Monty opens D2 (event B)
# to break it down further - you choose correctly, so Monty will open one of goat doors: either D2 (50%) or D3 (50%)
b_a2 = 0 # Monty won't open the door with the money, ever
b_a3 = 1 # the probability that the money is behind D3, and Monty chooses D2
# of course, this has to be 100% 
# you've chosen D1 (Monty won't open D1)
# the cash is behind D3 (Monty won't open to reveal the cash)

# marginal probabilities

# these are all the uninformed priors 
# in other words, before we know anything, there is a 1/3, 1/3, and 1/3 chance the money is behind each door
a1 = 1/3
a2 = 1/3
a3 = 1/3

# likelihood the money is behind d1
likelihood_d1 = pra1_bfunct(b_a1, a1, b_a2, a2, b_a3, a3)
print(likelihood_d1) # there is a 33% chance that you've chosen correctly

# but, what about door 3?

# write a function for Pr(A3|B)
pra3_bfunct = function(b_a1, a1, b_a2, a2, b_a3, a3) { # all the same arguments
  res = (b_a3*a3) / ((b_a3*a3) + (b_a2*a2) + (b_a1*a1))  # just a slightly different formula
  return(res)
}

# we can, of course, retain all of the same inputs (probabilistic components are the same)

# the output
likelihood_d3 = pra3_bfunct(b_a1, a1, b_a2, a2, b_a3, a3)
print(likelihood_d3) # there is a 67% chance that D3 has the money

#### Method III - Frequency-Based Simulation ####

simulate_fun = function(trials, switch_door = FALSE) {
  
  wins = 0 # initially, you have 0 wins; but, we will add to this
  
  for (i in 1:trials) { # for loop to run through each trial
    
    money_door = sample(1:3, 1) # one door, randomly selected, will have money
    choice_door = sample(1:3, 1) # one door is chosen by contestant
    # Monty reveals a door which does not have the money and the contestant hasn't chosen
    remaining_door = setdiff(1:3, c(money_door, choice_door))
    reveal_door = remaining_door[sample(length(remaining_door), 1)]
    
    # does the contestant switch?
    if (switch_door) {
      choice_door = setdiff(1:3, c(choice_door, reveal_door))
    }
    
    # does the contestant win?
    if (choice_door == money_door) {
      wins = wins + 1
    }
  }
  
  # the probability of winning
  return(wins/trials)
}

win_prob_no_switch = simulate_fun(trials = 10000, switch_door = FALSE)
win_prob_switch = simulate_fun(trials = 10000, switch_door = TRUE)

# the actual probabilities
win_perc_no_switch = win_prob_no_switch * 100
win_perc_yes_switch = win_prob_switch * 100

# note - these are two separate simulations, so will not be perfectly 100%
win_perc_no_switch
win_perc_yes_switch
