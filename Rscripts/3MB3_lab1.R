# =============================================

# Lab 1 - Session 1 (2014-09-09)

# David Champredon (champrd@math.mcmaster.ca)

# =============================================


# Saving a script in a file. No interactive session.


# Commented line start with the hash '#'
# and will be ignored by R


# --- Variables ---

# assign a value to a variable
a <- 2
a = 2
print(a)


str(a)

s="hello"
str(s)

# display
a
print(a)


# operations
b = sqrt(a) ; print(b)

b2 = sin(a*pi/4); print(b2)
bb = exp(b) ; bb

?sqrt
# Good help source:  http://stackoverflow.com



# --- VECTORS ---

# Creation

z = c(7,2,29,0)   # long-hand
# access an element
print(z[1])

i = 3
print(z[i])



# change the value of an element
i=2
z[2] = 51
print(z)



# shortcuts
za = c(1,2,3,4,5,6,7,8,9,10)
zb = c(1:10)
zc = 1:10
zd = seq(from=1, to =10, by=1)

z.odds = seq(from=1, to=9, by=2)

z.real = seq(from=sqrt(2), to=5*pi, by=log(2))

z.repeat = rep(x=3,times=10)

z.repeat2 = rep(x=c(3,7), times=5)

z.repeat3 = rep(x=c(3,7), times=c(8,2))



# Empty initialization

y = numeric(3)
print(y)

y[1] = 98
y[2] = 71
y[3] = 100
print(y)




# Warning!
y[4] = 82
print(y)

y[5]




# Accessing elements of vectors

a = z[3]

a=z[2]
b=z[3]
c=z[4]

b = z[2:4]
b = z[c(1,9,3)]

d = z[z>9]
d = z[z>3 & z<11]

l = (z>9)

# Setting values of elements

w = z

w[w>8] = 0



# --- Loops ---

y = numeric(1000)

for (i in 1:1000)
{
  y[i] = i*i-2
}
print(y)




i=1
while(i<=10)
{
  y[i] = i*i-2
  i = i+1
}


### Simple linear system

X = numeric(50)
X[1] = 0

for(i in 2:50)
{
	X[i] = 0.01 + 0.8*X[i-1]
}
print(X)


# -- Plots --

#par(mar=rep(0,4))
par(mfrow=c(1,1))

# Make up some data
myData = X

# Plot, naive
plot(myData)

?plot 
?par


# Plot, with explicit x-axis range
thetime <- 1:50 # defines the time steps
plot(x=thetime, y=myData)

# Plot, with proper labels
plot(x=thetime, y=myData, xlab="Time in hours", ylab="Size")

# Plot with lines style
plot(x=thetime, y=myData, xlab="Time in hours", ylab="Size",
     typ="l")

# Plot with lines and points style
plot(x=thetime, y=myData, xlab="Time in hours", ylab="Size",
     typ="o")

# Plot with lines and filled points style
plot(x=thetime, y=myData, xlab="Time in hours", ylab="Size",
     typ="o", pch=16)

# Plot with colours
plot(x=thetime, y=myData, xlab="Time in hours", ylab="Size",
     typ="o", pch=16, col="red")

# Plot thicker
plot(x=thetime, y=myData, xlab="Time in hours", ylab="Size",
     typ="o", pch=16, col="red", lwd=4)

# add grid (to the latest plot run)
grid()

?par


