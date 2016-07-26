private
safeVariableNameFor: aClass temporaries: allTempVars basedOn: aString
	| baseString i newString |
	newString := baseString := aString.
	i := 0.
	[ (allTempVars includes: newString) or: [ aClass definesInstanceVariable: newString ] ] 
		whileTrue: [ 
			i := i + 1.
			newString := baseString , i printString ].
	^ newString