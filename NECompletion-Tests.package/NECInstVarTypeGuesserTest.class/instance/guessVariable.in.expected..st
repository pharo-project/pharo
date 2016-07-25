private
guessVariable: aString in: aClass expected: theClassExpected 
	| typeGuesser result |
	typeGuesser := NECInstVarTypeGuesser 
		variableName: aString
		class: aClass.
	result := typeGuesser perform.
	self assert: result == theClassExpected