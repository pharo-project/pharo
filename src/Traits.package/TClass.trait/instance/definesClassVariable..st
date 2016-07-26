class variables
definesClassVariable: aGlobal
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	
	^ self classVariables includes: aGlobal