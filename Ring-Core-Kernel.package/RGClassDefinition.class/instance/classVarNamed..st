class variables
classVarNamed: aString

	^classVariables detect:[ :v| v name = aString asSymbol ] ifNone:[ nil ]