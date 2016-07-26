transforming
safeVariableNameBasedOn: aString 
	"Creates an unused variable name containing aString"

	| baseString newString i allTempVars |
	allTempVars := inlineParseTree allTemporaryVariables.
	baseString := aString copy.
	baseString at: 1 put: baseString first asLowercase.
	newString := baseString.
	i := 0.
	
	[(allTempVars includes: newString) 
		or: [class definesInstanceVariable: newString]] 
			whileTrue: 
				[i := i + 1.
				newString := baseString , i printString].
	^newString