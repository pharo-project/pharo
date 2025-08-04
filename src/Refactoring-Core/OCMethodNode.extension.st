Extension { #name : 'OCMethodNode' }

{ #category : '*Refactoring-Core' }
OCMethodNode >> addSelfReturn [
	^ body addSelfReturn
]

{ #category : '*Refactoring-Core' }
OCMethodNode >> extractSubtreeWith: aSourceCode [
	| extractedTree |
	extractedTree := self parserClass
		parseExpression: aSourceCode
		onError: [ :string :pos | ^ nil ].
	extractedTree ifNil: [ ^ nil ].
	(extractedTree isSequence and: [ extractedTree statements isEmpty ])
		ifTrue: [ ^ nil ].
	extractedTree := extractedTree isSequence
		ifTrue:
			[ OCParseTreeSearcher treeMatchingStatements: aSourceCode in: self ]
		ifFalse:
			[ OCParseTreeSearcher treeMatching: aSourceCode in: self ].	"or: [aTree isReturn]"
	^ extractedTree
		"ifNotNil: [ extractedTree parent: (self parentOfSubtree: extractedTree) ]"
]
