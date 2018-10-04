Extension { #name : #RBMethodNode }

{ #category : #'*Refactoring2-Transformations' }
RBMethodNode >> addPragma: aPragmaNode [

	pragmas ifNil: [ self pragmas: OrderedCollection new ].
	pragmas add: aPragmaNode
]

{ #category : #'*Refactoring2-Transformations' }
RBMethodNode >> allSequenceNodes [

	^ self allChildren select: #isSequence
]

{ #category : #'*Refactoring2-Transformations' }
RBMethodNode >> extractSubtreeWith: aSourceCode [

	| extractedTree |
	extractedTree := RBParser parseExpression: aSourceCode onError: [ :string :pos | ^ nil ].
	extractedTree ifNil: [ ^ nil ].
	
	(extractedTree isSequence 
	and: [extractedTree statements isEmpty]) ifTrue: [ ^ nil ].
	
	extractedTree := (extractedTree isSequence "or: [aTree isReturn]")
		ifTrue: [ RBParseTreeSearcher 
					 treeMatchingStatements: aSourceCode
					 in: self ]
		ifFalse: [ RBParseTreeSearcher
					  treeMatching: aSourceCode
					  in: self ].
			
	^ extractedTree ifNotNil: [ extractedTree parent: (self parentOfSubtree: extractedTree) ]
]

{ #category : #'*Refactoring2-Transformations' }
RBMethodNode >> parentOfSubtree: subtree [

	| sourceCode |
	sourceCode := subtree formattedCode.
	^ self allChildren reversed
		detect: [ :e | e formattedCode includesSubstring: sourceCode ]
		ifFound: [ :e | e parent ] ifNone: [ nil ]
]

{ #category : #'*Refactoring2-Transformations' }
RBMethodNode >> removePragma: aPragmaNode [

	pragmas remove: aPragmaNode ifAbsent: [ ]
]

{ #category : #'*Refactoring2-Transformations' }
RBMethodNode >> removeSubtree: aTree [

	^ aTree isReturn 
		ifTrue: [ RBParseTreeRewriter 
					 replace: aTree formattedCode
					 with: '' in: self
					 onInterval: aTree sourceInterval ]
		ifFalse: [ RBParseTreeRewriter 
					  replaceStatements: aTree formattedCode
		 			  with: '' in: self
					  onInterval: aTree sourceInterval ]
]

{ #category : #'*Refactoring2-Transformations' }
RBMethodNode >> variableWith: aSymbol [

	| matcher block |
	matcher := RBParseTreeSearcher new.
	block := [ :aNode :answer | aNode ].
	
	matcher
		matches: aSymbol do: block;
		matchesArgument: aSymbol do: block.
	^ matcher executeTree: self initialAnswer: nil
]
