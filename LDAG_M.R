library(dosearch)

#
# Variables
#
# D = Age group, gender, country of residence
# M = Purpose of the trip (contexts)
# X = Length of stay
# C = Main destination
# S = Quarter of departure
#
# Z = decisions before trip
#     * Means of transport for departure from Finland
#     * Main type of accommodation
#     * How many months prior to the trip was the first reservation made
#     * Travel group
#     * Overnight stays in secondary destinations
#
# W = decisions before or during trip
#     * (Did or experienced) nature experiences in Finland
#     * (Did or experienced) physical or sport activities outdoors in Finland
#     * (Did or experienced) wellbeing and relaxation activities in Finland
#     * (Did or experienced) cultural experiences in Finland
#     * (Did or experienced) a city break in Finland
#     * (Did or experienced) participation in a cultural or sport event in Finland
#     * (Did or experienced) shopping in Finland
#     * (Did or experienced) touring or road trip in Finland
#     * Traveled more than 50 kilometres (30 miles) in Finland
#
# Y = Expenditure
# U2,U1 = Unobserved factors

#
# Contexts
#
# M=0: (personal trips) 
#   * categrories: 1,4 
# M=1: (work-related trips) 
#   * categrories: 3,5,6,7,9



# LDAG

g <- "

I_X -> X

D -> M
D -> X : I_X = 1
D -> C 
D -> S 
D -> Z 
D -> W 
D -> Y

M -> X : I_X = 1
M -> C
M -> S
M -> Z
M -> W
M -> Y

X -> W : M = 0
X -> Z : M = 0
X -> Y

C -> W : M = 0
C -> Z : M = 0
C -> Y

S -> W : M = 0
S -> Z : M = 0
S -> Y

Z -> Y
Z -> W

W -> Y

U1 -> X : I_X = 1; M = 1
U1 -> C : M = 1
U1 -> S : M = 1
U1 -> Z
U1 -> W 

U2 -> X : I_X = 1
U2 -> S
U2 -> C

"

# Data source: survey
dat <- "
  P(Y,X,C,S,Z,W,D,M)
"

# Query: The effect of length of stay on expenditure
# P(Y|do(X))
query <- "P(Y|X,I_X=1)"
dosearch(dat, query, graph = g, control = list(heuristic = FALSE))

# Query: The effect of length of stay on expenditure conditioned on the context
# P(Y|do(X),M)
query <- "P(Y|X,I_X=1,M)"
dosearch(dat, query, graph = g, control = list(heuristic = FALSE))



# Separate D -> AG (age, gender) & L (country of residence) --------------------
# --> Does not affect on the identifiability

# LDAG
g <- "

I_X -> X

AG -> M
AG -> X : I_X = 1
AG -> C 
AG -> S 
AG -> Z 
AG -> W 
AG -> Y

L -> M
L -> X : I_X = 1
L -> C 
L -> S 
L -> Z 
L -> W 
L -> Y

M -> X : I_X = 1
M -> C
M -> S
M -> Z
M -> W
M -> Y

X -> W : M = 0
X -> Z : M = 0
X -> Y

C -> W : M = 0
C -> Z : M = 0
C -> Y

S -> W : M = 0
S -> Z : M = 0
S -> Y

Z -> Y
Z -> W

W -> Y

U1 -> X : I_X = 1; M = 1
U1 -> C : M = 1
U1 -> S : M = 1
U1 -> Z
U1 -> W 

U2 -> X : I_X = 1
U2 -> S
U2 -> C

"

# Data source: survey
dat <- "
  P(AG,L,M,X,C,S,Z,W,Y)
"
# P(Y|do(X))
query <- "P(Y|X,I_X=1)"
dosearch(dat, query, graph = g, control = list(heuristic = FALSE))

# P(Y|do(X),M)
query <- "P(Y|X,I_X=1,M)"
dosearch(dat, query, graph = g, control = list(heuristic = FALSE))



# Do-calculus ----------------------------------------------------------------

# DAG

g <- "

D -> M
D -> X
D -> C 
D -> S 
D -> Z 
D -> W 
D -> Y

M -> X
M -> C
M -> S
M -> Z
M -> W
M -> Y

X -> W
X -> Z
X -> Y

C -> W
C -> Z
C -> Y

S -> W
S -> Z
S -> Y

Z -> Y
Z -> W

W -> Y

U1 -> X
U1 -> C
U1 -> S
U1 -> Z
U1 -> W 

U2 -> X
U2 -> S
U2 -> C

"


# Data source: survey
dat <- "
  P(D,M,X,S,C,Z,W,Y)
"


# Queries:
query <- "P(Y|do(X))"
dosearch(dat, query, graph = g, control = list(heuristic = FALSE))
# The query P(Y|do(X)) is non-identifiable.

query <- "P(Y|do(X),M)"
dosearch(dat, query, graph = g, control = list(heuristic = FALSE))
# The query P(Y|do(X),M) is non-identifiable.
