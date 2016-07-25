recursive descent
quantifiedAtom: atom
	"Parse a quanitifer expression which can have one of the following forms
		{<min>,<max>}    match <min> to <max> occurences
		{<minmax>}       which is the same as with repeated limits: {<number>,<number>}
		{<min>,}         match at least <min> occurences
		{,<max>}         match maximally <max> occurences, which is the same as {0,<max>}"
	| min max |
	self next.
	lookahead = $,
		ifTrue: [ min := 0 ]
		ifFalse: [
			max := min := (self inputUpToAny: ',}' errorMessage: ' no terminating "}"') asUnsignedInteger ].
	lookahead = $,
		ifTrue: [
			self next.
			max := (self inputUpToAny: ',}' errorMessage: ' no terminating "}"') asUnsignedInteger ].	
	self match: $}.
	atom isNullable
		ifTrue: [ self signalNullableClosureParserError ].
	(max isNotNil and: [ max < min ])
		ifTrue: [ self signalParseError: ('wrong quantifier, expected ', min asString, ' <= ', max asString) ].
	^ RxsPiece new 
		initializeAtom: atom
		min: min
		max: max