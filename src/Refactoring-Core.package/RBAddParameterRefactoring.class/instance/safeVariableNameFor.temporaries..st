private
safeVariableNameFor: aClass temporaries: allTempVars 
	| baseString i newString |
	newString := baseString := 'anObject'.
	i := 0.
	
	[(allTempVars includes: newString) 
		or: [aClass definesInstanceVariable: newString]] 
			whileTrue: 
				[i := i + 1.
				newString := baseString , i printString].
	^newString