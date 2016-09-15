as yet unclassified
generateBlock: trailer withScope: scope
	| irTranslator |
      irTranslator := IRTranslatorV2 context: compilationContext trailer: trailer.
	irTranslator 
		pushOuterVectors: scope;
		visitNode: self;
		pragmas: pragmas.
	compiledMethod := irTranslator compiledBlock.
	^ compiledMethod