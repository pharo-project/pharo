"
ASTPragmaNode is an AST node that represents a method pragma.

We have a fixed set of allowed ""primitive"" pragma keywords. Every method implemented as a primitive call uses one of this pragmas.
And as we need some special treatment for methods implemented as primitive, the ASTPragmaNode adds the #isPrimitive testing method.

Instance Variables:
	arguments <SequenceableCollection of: ASTLiteralNode> our argument nodes
	left <Integer | nil> position of <
	right <Integer | nil> position of >
	selector	<Symbol>	the selector we're sending
	keywordsPositions	<IntegerArray | nil>	the positions of the selector keywords
"
Class {
	#name : 'OCPragmaNode',
	#superclass : 'OCProgramNode',
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
OCPragmaNode class >> selector: aSymbol arguments: valueNodes [
	^ self selector: aSymbol keywordsPositions: nil arguments: valueNodes
]

{ #category : 'instance creation' }
OCPragmaNode class >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	^ self new selector: aSymbol keywordsPositions: positionList arguments: valueNodes
]

{ #category : 'comparing' }
OCPragmaNode >> = anObject [
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
OCPragmaNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitPragmaNode: self
]

{ #category : 'accessing' }
OCPragmaNode >> argumentAt: keywordSelector [
	^ self
		argumentAt: keywordSelector
		ifAbsent: [ KeyNotFound signalFor: keywordSelector ]
]

{ #category : 'accessing' }
OCPragmaNode >> argumentAt: keywordSelector ifAbsent: absentBlock [
	self selectorParts with: self arguments do: [ :selectorPart :argument |
		selectorPart value = keywordSelector
			ifTrue: [ ^ argument ]].
	^ absentBlock value
]

{ #category : 'accessing' }
OCPragmaNode >> arguments [
	^ arguments
]

{ #category : 'accessing' }
OCPragmaNode >> arguments: aLiteralCollection [
	arguments := aLiteralCollection.
	arguments do: [ :each | each parent: self ]
]

{ #category : 'accessing' }
OCPragmaNode >> children [
	^ arguments
]

{ #category : 'matching' }
OCPragmaNode >> copyInContext: aDictionary [
	^ self class
		selector: self selector
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ])
]

{ #category : 'testing' }
OCPragmaNode >> defines: aName [
	^ self isPrimitive and: [ arguments anySatisfy: [ :each | each value = aName ] ]
]

{ #category : 'comparing' }
OCPragmaNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [ ^ false ].
	self selector = anObject selector ifFalse: [ ^ false ].
	self arguments size = anObject arguments size ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		(first equalTo: second withMapping: aDictionary)
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'comparing' }
OCPragmaNode >> hash [
	^ self selector hash bitXor: (self hashForCollection: self arguments)
]

{ #category : 'initialization' }
OCPragmaNode >> initialize [
	super initialize.

	arguments := #().
	keywordsPositions := #().
	left := 0.
	right := 0
]

{ #category : 'testing' }
OCPragmaNode >> isBinary [
	^ (self isUnary or: [self isKeyword]) not
]

{ #category : 'errors' }
OCPragmaNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self arguments anySatisfy: [:each | each isFaulty]
]

{ #category : 'testing' }
OCPragmaNode >> isKeyword [
	^ selector isKeyword
]

{ #category : 'testing' }
OCPragmaNode >> isPragma [
	^ true
]

{ #category : 'testing' }
OCPragmaNode >> isPrimitive [
	^ #(primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:) includes: self selector
]

{ #category : 'testing' }
OCPragmaNode >> isUnary [
	^ arguments isEmpty
]

{ #category : 'accessing' }
OCPragmaNode >> keywordsIntervals [
	^selector keywords
		with: self keywordsPositions
		collect: [:keyword :start| start to: (start = 0 ifTrue: [ -1 ] ifFalse: [ start + keyword size - 1]) ]
]

{ #category : 'accessing' }
OCPragmaNode >> keywordsPositions [
	^keywordsPositions ifNil: [ (selector keywords collect: [:char| 0 ]) asIntegerArray ]
]

{ #category : 'accessing' }
OCPragmaNode >> keywordsPositions: aPositionsList [
	keywordsPositions := aPositionsList ifNotNil: [:list| list asIntegerArray ]
]

{ #category : 'accessing - token' }
OCPragmaNode >> left [
	^ left
]

{ #category : 'accessing - token' }
OCPragmaNode >> left: anInteger [
	left := anInteger
]

{ #category : 'matching' }
OCPragmaNode >> match: aNode inContext: aDictionary [
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
OCPragmaNode >> numArgs [
	^ self selector numArgs
]

{ #category : 'copying' }
OCPragmaNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ])
]

{ #category : 'accessing' }
OCPragmaNode >> pragma [

	^ pragma
]

{ #category : 'accessing' }
OCPragmaNode >> pragma: anObject [

	pragma := anObject
]

{ #category : 'replacing' }
OCPragmaNode >> replaceNode: aNode withNode: anotherNode [
	self arguments: (arguments collect: [ :each |
		each == aNode
			ifTrue: [ anotherNode ]
			ifFalse: [ each ] ])
]

{ #category : 'accessing - token' }
OCPragmaNode >> right [
	^ right
]

{ #category : 'accessing - token' }
OCPragmaNode >> right: anInteger [
	right := anInteger
]

{ #category : 'accessing' }
OCPragmaNode >> selector [
	^ selector
]

{ #category : 'accessing' }
OCPragmaNode >> selector: aSelector [
	keywordsPositions := nil.
	selector := aSelector asSymbol
]

{ #category : 'initialization' }
OCPragmaNode >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	aSymbol numArgs == valueNodes size
		ifFalse:
			[self error: 'Attempting to assign selector with wrong number of arguments.'].

	self
		arguments: valueNodes;
		selector: aSymbol;
		keywordsPositions: positionList
]

{ #category : 'accessing' }
OCPragmaNode >> selectorParts [
	^ selector keywords collect: [:keyword| keyword asSymbol ]
]

{ #category : 'accessing' }
OCPragmaNode >> sentMessages [
	^ super sentMessages
		add: self selector;
		yourself
]

{ #category : 'accessing' }
OCPragmaNode >> start [
	^ left
]

{ #category : 'accessing' }
OCPragmaNode >> stop [
	^ right
]
