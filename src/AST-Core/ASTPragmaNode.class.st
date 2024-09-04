"
RBPragmaNode is an AST node that represents a method pragma.

We have a fixed set of allowed ""primitive"" pragma keywords. Every method implemented as a primitive call uses one of this pragmas.
And as we need some special treatment for methods implemented as primitive, the RBPragmaNode adds the #isPrimitive testing method.

Instance Variables:
	arguments <SequenceableCollection of: ASTLiteralNode> our argument nodes
	left <Integer | nil> position of <
	right <Integer | nil> position of >
	selector	<Symbol>	the selector we're sending
	keywordsPositions	<IntegerArray | nil>	the positions of the selector keywords
"
Class {
	#name : 'ASTPragmaNode',
	#superclass : 'ASTProgramNode',
	#instVars : [
		'selector',
		'keywordsPositions',
		'arguments',
		'left',
		'right',
		'pragma'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
ASTPragmaNode class >> selector: aSymbol arguments: valueNodes [
	^ self selector: aSymbol keywordsPositions: nil arguments: valueNodes
]

{ #category : 'instance creation' }
ASTPragmaNode class >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	^ self new selector: aSymbol keywordsPositions: positionList arguments: valueNodes
]

{ #category : 'comparing' }
ASTPragmaNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self selector = anObject selector ifFalse: [^false].
	self arguments size = anObject arguments size ifFalse: [^false].
	self arguments
		with: anObject arguments
		do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : 'visiting' }
ASTPragmaNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitPragmaNode: self
]

{ #category : 'accessing' }
ASTPragmaNode >> argumentAt: keywordSelector [
	^ self
		argumentAt: keywordSelector
		ifAbsent: [ KeyNotFound signalFor: keywordSelector ]
]

{ #category : 'accessing' }
ASTPragmaNode >> argumentAt: keywordSelector ifAbsent: absentBlock [
	self selectorParts with: self arguments do: [ :selectorPart :argument |
		selectorPart value = keywordSelector
			ifTrue: [ ^ argument ]].
	^ absentBlock value
]

{ #category : 'accessing' }
ASTPragmaNode >> arguments [
	^ arguments
]

{ #category : 'accessing' }
ASTPragmaNode >> arguments: aLiteralCollection [
	arguments := aLiteralCollection.
	arguments do: [ :each | each parent: self ]
]

{ #category : 'accessing' }
ASTPragmaNode >> children [
	^ arguments
]

{ #category : 'matching' }
ASTPragmaNode >> copyInContext: aDictionary [
	^ self class
		selector: self selector
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ])
]

{ #category : 'testing' }
ASTPragmaNode >> defines: aName [
	^ self isPrimitive and: [ arguments anySatisfy: [ :each | each value = aName ] ]
]

{ #category : 'comparing' }
ASTPragmaNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [ ^ false ].
	self selector = anObject selector ifFalse: [ ^ false ].
	self arguments size = anObject arguments size ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		(first equalTo: second withMapping: aDictionary)
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'comparing' }
ASTPragmaNode >> hash [
	^ self selector hash bitXor: (self hashForCollection: self arguments)
]

{ #category : 'initialization' }
ASTPragmaNode >> initialize [
	super initialize.

	arguments := #().
	keywordsPositions := #().
	left := 0.
	right := 0
]

{ #category : 'testing' }
ASTPragmaNode >> isBinary [
	^ (self isUnary or: [self isKeyword]) not
]

{ #category : 'errors' }
ASTPragmaNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self arguments anySatisfy: [:each | each isFaulty]
]

{ #category : 'testing' }
ASTPragmaNode >> isKeyword [
	^ selector isKeyword
]

{ #category : 'testing' }
ASTPragmaNode >> isPragma [
	^ true
]

{ #category : 'testing' }
ASTPragmaNode >> isPrimitive [
	^ #(primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:) includes: self selector
]

{ #category : 'testing' }
ASTPragmaNode >> isUnary [
	^ arguments isEmpty
]

{ #category : 'accessing' }
ASTPragmaNode >> keywordsIntervals [
	^selector keywords
		with: self keywordsPositions
		collect: [:keyword :start| start to: (start = 0 ifTrue: [ -1 ] ifFalse: [ start + keyword size - 1]) ]
]

{ #category : 'accessing' }
ASTPragmaNode >> keywordsPositions [
	^keywordsPositions ifNil: [ (selector keywords collect: [:char| 0 ]) asIntegerArray ]
]

{ #category : 'accessing' }
ASTPragmaNode >> keywordsPositions: aPositionsList [
	keywordsPositions := aPositionsList ifNotNil: [:list| list asIntegerArray ]
]

{ #category : 'accessing - token' }
ASTPragmaNode >> left [
	^ left
]

{ #category : 'accessing - token' }
ASTPragmaNode >> left: anInteger [
	left := anInteger
]

{ #category : 'matching' }
ASTPragmaNode >> match: aNode inContext: aDictionary [
	aNode class = self class
		ifFalse: [ ^ false ].
	self selector = aNode selector
		ifFalse: [ ^ false ].
	self arguments with: aNode arguments do: [ :first :second |
		(first match: second inContext: aDictionary)
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'accessing' }
ASTPragmaNode >> numArgs [
	^ self selector numArgs
]

{ #category : 'copying' }
ASTPragmaNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ])
]

{ #category : 'accessing' }
ASTPragmaNode >> pragma [

	^ pragma
]

{ #category : 'accessing' }
ASTPragmaNode >> pragma: anObject [

	pragma := anObject
]

{ #category : 'replacing' }
ASTPragmaNode >> replaceNode: aNode withNode: anotherNode [
	self arguments: (arguments collect: [ :each |
		each == aNode
			ifTrue: [ anotherNode ]
			ifFalse: [ each ] ])
]

{ #category : 'accessing - token' }
ASTPragmaNode >> right [
	^ right
]

{ #category : 'accessing - token' }
ASTPragmaNode >> right: anInteger [
	right := anInteger
]

{ #category : 'accessing' }
ASTPragmaNode >> selector [
	^ selector
]

{ #category : 'accessing' }
ASTPragmaNode >> selector: aSelector [
	keywordsPositions := nil.
	selector := aSelector asSymbol
]

{ #category : 'initialization' }
ASTPragmaNode >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	aSymbol numArgs == valueNodes size
		ifFalse:
			[self error: 'Attempting to assign selector with wrong number of arguments.'].

	self
		arguments: valueNodes;
		selector: aSymbol;
		keywordsPositions: positionList
]

{ #category : 'accessing' }
ASTPragmaNode >> selectorParts [
	^ selector keywords collect: [:keyword| keyword asSymbol ]
]

{ #category : 'accessing' }
ASTPragmaNode >> sentMessages [
	^ super sentMessages
		add: self selector;
		yourself
]

{ #category : 'accessing' }
ASTPragmaNode >> start [
	^ left
]

{ #category : 'accessing' }
ASTPragmaNode >> stop [
	^ right
]
