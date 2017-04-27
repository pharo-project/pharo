private
behaviorNamed: aSymbol ifPresent: presentBlock

	^ (self environment classOrTraitNamed: aSymbol) 
		ifNil: [ self ]
		ifNotNil: presentBlock