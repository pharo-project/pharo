private
behaviorNamed: aSymbol ifPresent: presentBlock

	^ (environment classOrTraitNamed: aSymbol) 
		ifNil: [ self ]
		ifNotNil: presentBlock