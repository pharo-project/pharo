primitives
indexOfAscii: anInteger inString: aString startingAt: start	
	start to: aString size do: [ :index |
		(aString basicAt: index) = anInteger ifTrue: [ ^index ] ].
	^0

