Extension { #name : 'ASTProgramNode' }

{ #category : '*NECompletion' }
ASTProgramNode >> completionEntries: offset [
	"for now we give all selectors and gloabls as a fallback"
	| selectors globals |
	self methodNode sourceCode ifNil: [ ^#() ].
	selectors := Symbol selectorTable
		select: [ :each | each beginsWith: (self completionToken: offset)]
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ].

	globals := Smalltalk globals keys
			select: [ :each | each beginsWith: (self completionToken: offset)]
			thenCollect: [ :each | NECGlobalEntry contents: each  node: self ].

	^ selectors , globals
]

{ #category : '*NECompletion' }
ASTProgramNode >> completionToken: offset [
	"fall back: if we are not able to parse to something better, we use the word before the cursor"

	^ self methodNode sourceCode
		  ifNil: [ String new ]
		  ifNotNil: [ :source | source wordBefore: offset ]
]
