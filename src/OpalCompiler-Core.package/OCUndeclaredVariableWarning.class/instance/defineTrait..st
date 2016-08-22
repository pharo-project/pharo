correcting
defineTrait: traitName 
	"Prompts the user to define a new trait."
	
	| traitSymbol systemCategory traitDefinition |
	traitSymbol := traitName asSymbol.
	systemCategory := self methodClass category
		ifNil: [ 'Unknown' ].
	traitDefinition := 'Trait named: #' , traitSymbol , '
		uses:{} 
		package: ''' , systemCategory , ''''.
	traitDefinition := UIManager default 
		multiLineRequest: 'Edit trait definition:'
		initialAnswer: traitDefinition
		answerHeight: 150.
	(traitDefinition isNil or: [ traitDefinition isEmpty ])
		ifTrue: [ ^ Abort signal ].
	self class compiler
		source: traitDefinition;
		logged: true;
		evaluate.
	^ (node owningScope lookupVar: traitSymbol)
		ifNil: [self error: 'should be not happen']