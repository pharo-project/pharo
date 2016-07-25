utils
guessVariable: aString in: aClass expected: theClassExpected 
	| typeGuesser result |
	typeGuesser := NECClassVarTypeGuesser 
		variableName: aString
		class: aClass.
	result := typeGuesser perform.
	self assert: result == theClassExpected