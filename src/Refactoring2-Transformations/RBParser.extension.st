Extension { #name : #RBParser }

{ #category : #'*Refactoring2-Transformations' }
RBParser class >> parsePragma: aString [

	| parser |
	parser := self new.
	parser initializeParserWith: aString.
	^ parser parsePragmaAndReturn
]

{ #category : #'*Refactoring2-Transformations' }
RBParser >> parsePragmaAndReturn [

	self parsePragma.
	^ pragmas first
]
