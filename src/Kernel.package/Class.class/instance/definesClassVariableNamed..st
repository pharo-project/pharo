class variables
definesClassVariableNamed: aString
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	
	^ self classVarNames includes: aString