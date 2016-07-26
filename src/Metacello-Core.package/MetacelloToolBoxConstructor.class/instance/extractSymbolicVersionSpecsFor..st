extraction
extractSymbolicVersionSpecsFor: sourceVersionSymbol
	| coll versionSpec pragma |
	coll := self extractSymbolicVersionPragmas at: sourceVersionSymbol ifAbsent: [ ^ #() ].
	coll size > 1
		ifTrue: [ self error: 'More than one pragma defining ' , sourceVersionSymbol printString ].
	pragma := coll at: 1.
	self evaluatePragma: pragma.
	^ self methodSections