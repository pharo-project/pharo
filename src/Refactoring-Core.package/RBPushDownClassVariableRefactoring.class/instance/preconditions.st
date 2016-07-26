preconditions
preconditions
	"Preconditions are that only one subclass refers to the class variable."

	^(RBCondition definesClassVariable: variableName in: class) 
		& (RBCondition withBlock: 
					[self findDestinationClass.
					true])