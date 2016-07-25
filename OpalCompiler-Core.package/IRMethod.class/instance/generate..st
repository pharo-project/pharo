translating
generate: trailer
	
	| irTranslator |
   irTranslator := IRTranslatorV2 context: compilationContext trailer: trailer.
	irTranslator 
		visitNode: self;
		pragmas: pragmas.
	compiledMethod := irTranslator compiledMethod.
	self sourceNode 
		ifNotNil: [ 
			compiledMethod classBinding: self sourceNode methodClass binding.
			compiledMethod selector: self sourceNode selector ]
		ifNil: [ 
			compiledMethod classBinding: UndefinedObject binding.
			compiledMethod selector: #UndefinedMethod ].
	^ compiledMethod