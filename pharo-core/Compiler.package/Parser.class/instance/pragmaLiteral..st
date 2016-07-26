pragmas
pragmaLiteral: selectorSoFar
	"Read a pragma literal.  As a nicety we allow a variable name (rather
	 than a literal string) as the second argument to primitive:error:"

	(hereType == #string or: [ hereType == #literal or: [ hereType == #number ] ])
		ifTrue: [ ^ self advance ].
	(here == $# and: [ tokenType == #word ])
		ifTrue: [ ^ self advance ].
	(here == #- and: [ tokenType == #number ])
		ifTrue: [ ^ (self advance; advance) negated ].
	(here = 'true' or: [ here = 'false' or: [ here = 'nil' ] ])
		ifTrue: [ ^ self class compiler evaluate: self advance ].
	"This nicety allows one to supply a primitive error
	 temp as a variable name, rather than a string."
	((selectorSoFar beginsWith: 'primitive:')
	 and: [(selectorSoFar endsWith: 'error:')
	 and: [hereType == #word]]) ifTrue:
		[^self advance].
	^self expected: 'Literal constant'