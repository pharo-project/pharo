validating
checkValidName: aString 
	"aString first canBeGlobalVarInitial
		ifFalse: [InvalidGlobalName signal: 'Class name does not start with a valid Global Var Initial' for: aString]"