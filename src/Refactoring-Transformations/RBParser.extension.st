Extension { #name : 'RBParser' }

{ #category : '*Refactoring-Transformations' }
RBParser >> parsePragmaAndReturn [

	self parsePragma.
	^ pragmas first
]
