Extension { #name : #ASTParser }

{ #category : #'*Refactoring2-Transformations' }
ASTParser >> parsePragmaAndReturn [

	self parsePragma.
	^ pragmas first
]
