comparing
equals: aContext
	aContext ifNil: [ ^ false ].
   ^ self matches: aContext search