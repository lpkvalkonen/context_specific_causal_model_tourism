library(cfid)


gm0text <- "

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

X -> Y

C -> Y

S -> Y

Z -> Y

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

gm0text_Mremoved <- "

D -> X
D -> C 
D -> S 
D -> Z 
D -> W 
D -> Y

X -> Y

C -> Y

S -> Y

Z -> Y

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


gm1text <- "
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

S -> Z 
S -> W 
S -> Y

Z -> Y
Z -> W

W -> Y

U1 -> Z
U1 -> W 

U2 -> X 
U2 -> S
U2 -> C
"

gm1text_Mremoved <- "
D -> X 
D -> C 
D -> S 
D -> Z 
D -> W 
D -> Y

X -> W 
X -> Z 
X -> Y

C -> W 
C -> Z 
C -> Y

S -> Z 
S -> W 
S -> Y

Z -> Y
Z -> W

W -> Y

U1 -> Z
U1 -> W 

U2 -> X 
U2 -> S
U2 -> C
"


gm0 <- dag(gm0text,c("U1","U2"))
gm1 <- dag(gm1text,c("U1","U2"))
gm0_Mremoved <- dag(gm0text_Mremoved,c("U1","U2"))
gm1_Mremoved <- dag(gm1text_Mremoved,c("U1","U2"))

ydox0 <- cf(var = "Y", obs = 0L, sub = c(X = 0L))
ydox1 <- cf(var = "Y", obs = 0L, sub = c(X = 1L))
xobs1 <- cf(var = "X", obs = c(X = 1L))
xobs0 <- cf(var = "X", obs = c(X = 0L))
yobs1 <- cf(var = "Y", obs = c(Y = 1L))
yobs0 <- cf(var = "Y", obs = c(Y = 0L))

# Estimands of interest
identifiable(gm0, gamma = ydox1, delta = xobs0, data = "observations")
identifiable(gm1, gamma = ydox1, delta = xobs0, data = "observations")

# Estimand of interest when M is removed from the graph.
identifiable(gm0_Mremoved, gamma = ydox1, delta = xobs0, data = "observations")
identifiable(gm1_Mremoved, gamma = ydox1, delta = xobs0, data = "observations")

# Counterfactuals where we condition also on the total expenditure cannot be identified
identifiable(gm0, gamma = ydox1, delta = conj(xobs0,yobs0), data = "observations")
identifiable(gm1, gamma = ydox1, delta = conj(xobs0,yobs0), data = "observations")
