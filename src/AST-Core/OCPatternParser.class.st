"
RBPatternParser is a subclass of RBParser that allows the extended syntax that creates matching trees. These trees can be used by the ParseTreeMatcher to search and transform source code.

"
Class {
	#name : 'OCPatternParser',
	#superclass : 'OCParser',
	#category : 'AST-Core-Parser',
	#package : 'AST-Core',
	#tag : 'Parser'
}

{ #category : 'private - classes' }
OCPatternParser >> messageNodeClass [
	^OCPatternMessageNode
]

{ #category : 'private - classes' }
OCPatternParser >> methodNodeClass [
	^OCPatternMethodNode
]

{ #category : 'private - parsing' }
OCPatternParser >> parseError: aString [

	self error: aString
]

{ #category : 'private - parsing' }
OCPatternParser >> parseLiteralByteArrayObject [
	| node |
	(currentToken isIdentifier and: [currentToken isPatternVariable])
		ifTrue:
			[node := self variableNodeClass identifierNamed: currentToken value at: currentToken start.
			node isLiteralNode
				ifTrue:
					[self step.
					^node]].
	^super parseLiteralByteArrayObject
]

{ #category : 'private - parsing' }
OCPatternParser >> parsePatternBlock: aClass [
	| position node |
	position := currentToken start.
	self step.
	node := aClass new.
	self parseBlockArgsInto: node.
	node left: position.

	node body: self sequenceNodeClass new.
	self
		parseStatements: false
		into: node body
		untilAnyCloserOf: '}'.

	(currentToken isSpecial: $})
		ifFalse: [ self addParserError: '''}'' expected' to: node body.
			^ node ].
	node right: currentToken start.
	self step.
	^node
]

{ #category : 'private - parsing' }
OCPatternParser >> parsePragmaLiteral [
	| node |
	currentToken isPatternBlock
		ifTrue: [ ^ self parsePatternBlock: OCPatternBlockNode ].
	(currentToken isIdentifier and: [ currentToken isPatternVariable ]) ifTrue: [
		node := self variableNodeClass identifierNamed: currentToken value at: currentToken start.
		node isLiteralNode
			ifFalse: [ self parseError: 'Literal pattern expected' ].
		self step.
		currentToken isPatternBlock ifTrue: [
			node := (self parsePatternBlock: OCPatternWrapperBlockNode)
				wrappedNode: node;
				yourself ].
		^ node ].
	^ super parsePragmaLiteral
]

{ #category : 'private - parsing' }
OCPatternParser >> parsePrimitiveLiteral [
	| node |
	(currentToken isIdentifier and: [currentToken isPatternVariable])
		ifTrue:
			[node := self variableNodeClass identifierNamed: currentToken value at: currentToken start.
			node isLiteralNode
				ifTrue:
					[self step.
					^node].
			currentToken := OCLiteralToken
						value: currentToken value asSymbol
						start: currentToken start
						stop: currentToken stop].
	^super parsePrimitiveLiteral
]

{ #category : 'private - parsing' }
OCPatternParser >> parsePrimitiveObject [
	currentToken isPatternBlock
		ifTrue: [^self parsePatternBlock: OCPatternBlockNode].
	^super parsePrimitiveObject
]

{ #category : 'private - parsing' }
OCPatternParser >> parseUnaryMessage [
	| node |
	node := self parsePrimitiveObject.
	self addCommentsTo: node.

	[currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	currentToken isPatternBlock
		ifTrue:
			[node := self parseWrapperPatternBlockWith: node ].
	currentToken isIdentifier]
			whileTrue: [
				currentToken isKeywordPattern
					ifTrue: [ self parseErrorNode: ' incomplete keyword pattern ' ].
				node := self parseUnaryMessageWith: node].
	self addCommentsTo: node.
	^node
]

{ #category : 'private - parsing' }
OCPatternParser >> parseWrapperPatternBlockWith: wrappedNode [
	| node |

	node := self parsePatternBlock: OCPatternWrapperBlockNode.
	node class == OCPatternWrapperBlockNode ifTrue: [
		self flag: 'I think this is ugly, but I don''t know how to avoid checking the result of #parsePatternBlock:'.
		node wrappedNode: wrappedNode ].

	^ node
]

{ #category : 'private - classes' }
OCPatternParser >> pragmaNodeClass [
	^OCPatternPragmaNode
]

{ #category : 'accessing' }
OCPatternParser >> scannerClass [
	^OCPatternScanner
]

{ #category : 'private - classes' }
OCPatternParser >> variableNodeClass [
	^OCPatternVariableNode
]
