instance variables
instanceVariableNamed: aString
	^ instanceVariables 
		detect: [ :v | v name = aString asSymbol ] 
		ifNone: [ nil ]