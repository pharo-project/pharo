utilities
validClassName: aString 
	"Class names and class variable names have the same restrictions"

	^self checkClassVarName: aString in: self