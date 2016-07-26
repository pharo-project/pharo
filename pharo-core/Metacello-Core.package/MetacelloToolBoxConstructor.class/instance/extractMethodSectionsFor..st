extraction
extractMethodSectionsFor: sourceVersionString
	| coll pragma |
	coll := self extractAllVersionPragmas at: sourceVersionString ifAbsent: [ ^ #() ].
	coll size > 1
		ifTrue: [ self error: 'More than one pragma defining ' , sourceVersionString printString ].
	pragma := coll at: 1.
	self evaluatePragma: pragma.
	self methodSections do: [ :methodSection | self evaluateMethodSection: methodSection version: sourceVersionString ].