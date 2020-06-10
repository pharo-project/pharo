Extension { #name : #RBParser }

{ #category : #'*Refactoring2-Transformations' }
RBParser >> parsePragmaAndReturn [

	self parsePragma.
	^ pragmas first
]
