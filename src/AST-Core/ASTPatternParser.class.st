"
ASTPatternParser is a subclass of ASTParser that allows the extended syntax that creates matching trees. These trees can be used by the ParseTreeMatcher to search and transform source code.

"
Class {
	#name : #ASTPatternParser,
	#superclass : #ASTParser,
	#category : #'AST-Core-Parser'
}

{ #category : #'private - classes' }
ASTPatternParser >> messageNodeClass [
	^RBPatternMessageNode
]

{ #category : #'private - classes' }
ASTPatternParser >> methodNodeClass [
	^ASTPatternMethodNode
]

{ #category : #'private - parsing' }
ASTPatternParser >> parseError: aString [

	self error: aString
]

{ #category : #'private - parsing' }
ASTPatternParser >> parseLiteralByteArrayObject [
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

{ #category : #'private - parsing' }
ASTPatternParser >> parsePatternBlock: aClass [ 
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
		until: [ currentToken isSpecial
			and: [ currentToken value = $} ] ].
	
	(currentToken isSpecial and: [currentToken value = $}]) 
		ifFalse: [ self addParserError: '''}'' expected' to: node body.
			^ node ].
	node right: currentToken start.
	self step.
	^node
]

{ #category : #'private - parsing' }
ASTPatternParser >> parsePragmaLiteral [
	| node |
	currentToken isPatternBlock 
		ifTrue: [ ^ self parsePatternBlock: RBPatternBlockNode ].
	(currentToken isIdentifier and: [ currentToken isPatternVariable ]) ifTrue: [
		node := self variableNodeClass identifierNamed: currentToken value at: currentToken start.
		node isLiteralNode 
			ifFalse: [ self parseError: 'Literal pattern expected' ].
		self step.
		currentToken isPatternBlock ifTrue: [
			node := (self parsePatternBlock: RBPatternWrapperBlockNode)
				wrappedNode: node;
				yourself ].
		^ node ].
	^ super parsePragmaLiteral
]

{ #category : #'private - parsing' }
ASTPatternParser >> parsePrimitiveLiteral [
	| node |
	(currentToken isIdentifier and: [currentToken isPatternVariable]) 
		ifTrue: 
			[node := self variableNodeClass identifierNamed: currentToken value at: currentToken start.
			node isLiteralNode 
				ifTrue: 
					[self step.
					^node].
			currentToken := ASTLiteralToken 
						value: currentToken value asSymbol
						start: currentToken start
						stop: currentToken stop].
	^super parsePrimitiveLiteral
]

{ #category : #'private - parsing' }
ASTPatternParser >> parsePrimitiveObject [
	currentToken isPatternBlock 
		ifTrue: [^self parsePatternBlock: RBPatternBlockNode].
	^super parsePrimitiveObject
]

{ #category : #'private - parsing' }
ASTPatternParser >> parseUnaryMessage [
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
					ifTrue: [ self parserError: ' incomplete keyword pattern ' ].
				node := self parseUnaryMessageWith: node].
	self addCommentsTo: node.
	^node
]

{ #category : #'private - parsing' }
ASTPatternParser >> parseWrapperPatternBlockWith: wrappedNode [
	| node |

	node := self parsePatternBlock: RBPatternWrapperBlockNode.
	node class == RBPatternWrapperBlockNode ifTrue: [ 
		self flag: 'I think this is ugly, but I don''t know how to avoid checking the result of #parsePatternBlock:'.
		node wrappedNode: wrappedNode ].
	
	^ node
]

{ #category : #private }
ASTPatternParser >> patchLiteralArrayToken [
	(currentToken isIdentifier and: [currentToken isPatternVariable]) 
		ifTrue: [^self].
	super patchLiteralArrayToken
]

{ #category : #'private - classes' }
ASTPatternParser >> pragmaNodeClass [
	^ASTPatternPragmaNode
]

{ #category : #accessing }
ASTPatternParser >> scannerClass [
	^ASTPatternScanner
]

{ #category : #'private - classes' }
ASTPatternParser >> variableNodeClass [
	^ASTPatternVariableNode
]
