class variables
hasClassVariable: aGlobal
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	
	^ self classVariables identityIncludes: aGlobal