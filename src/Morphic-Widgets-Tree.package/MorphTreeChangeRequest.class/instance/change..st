accessing
change: aTreeMorph
	action ifNil: [^self].
	aTreeMorph perform: action withEnoughArguments: self arguments