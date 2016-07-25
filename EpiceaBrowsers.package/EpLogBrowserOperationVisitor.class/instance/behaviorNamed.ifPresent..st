private
behaviorNamed: aSymbol ifPresent: presentBlock

	^ (environment classOrTraitNamed: aSymbol) 
		ifNil: [ self inform: 'Cannot find ', aSymbol ]
		ifNotNil: presentBlock