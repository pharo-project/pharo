"
RBPragmaNode is an AST node that represents a method pragma.

We have a fixed set of allowed ""primitive"" pragma keywords. Every method implemented as a primitive call uses one of this pragmas.
And as we need some special treatment for methods implemented as primitive, the RBPragmaNode adds the #isPrimitive testing method.

Instance Variables:
	arguments <SequenceableCollection of: RBLiteralNode> our argument nodes
	left <Integer | nil> position of <
	right <Integer | nil> position of >
	selector	<Symbol>	the selector we're sending
	keywordsPositions	<IntegerArray | nil>	the positions of the selector keywords
"
Class {
	#name : #RBPragmaNode,
	#superclass : #RBProgramNode,
	#instVars : [
		'selector',
		'keywordsPositions',
		'arguments',
		'left',
		'right'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBPragmaNode class >> selector: aSymbol arguments: valueNodes [
	^ self selector: aSymbol keywordsPositions: nil arguments: valueNodes
]

{ #category : #'instance creation' }
RBPragmaNode class >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	^ self new selector: aSymbol keywordsPositions: positionList arguments: valueNodes
]

{ #category : #comparing }
RBPragmaNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self selector = anObject selector ifFalse: [^false].
	self arguments size = anObject arguments size ifFalse: [^false].
	self arguments
		with: anObject arguments
		do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : #visiting }
RBPragmaNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitPragmaNode: self
]

{ #category : #accessing }
RBPragmaNode >> argumentAt: keywordSelector [
	^ self
		argumentAt: keywordSelector
		ifAbsent: [ KeyNotFound signalFor: keywordSelector ]
]

{ #category : #accessing }
RBPragmaNode >> argumentAt: keywordSelector ifAbsent: absentBlock [
	self selectorParts with: self arguments do: [ :selectorPart :argument |
		selectorPart value = keywordSelector
			ifTrue: [ ^ argument ]].
	^ absentBlock value
]

{ #category : #accessing }
RBPragmaNode >> arguments [
	^ arguments
]

{ #category : #accessing }
RBPragmaNode >> arguments: aLiteralCollection [
	arguments := aLiteralCollection.
	arguments do: [ :each | each parent: self ]
]

{ #category : #accessing }
RBPragmaNode >> children [
	^ arguments
]

{ #category : #matching }
RBPragmaNode >> copyInContext: aDictionary [
	^ self class
		selector: self selector
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ])
]

{ #category : #testing }
RBPragmaNode >> defines: aName [
	^ self isPrimitive and: [ arguments anySatisfy: [ :each | each value = aName ] ]
]

{ #category : #comparing }
RBPragmaNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [ ^ false ].
	self selector = anObject selector ifFalse: [ ^ false ].
	self arguments size = anObject arguments size ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		(first equalTo: second withMapping: aDictionary)
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #comparing }
RBPragmaNode >> hash [
	^ self selector hash bitXor: (self hashForCollection: self arguments)
]

{ #category : #initialization }
RBPragmaNode >> initialize [
	super initialize.

	arguments := #().
	keywordsPositions := #().
	left := 0.
	right := 0
]

{ #category : #testing }
RBPragmaNode >> isBinary [
	^ (self isUnary or: [self isKeyword]) not
]

{ #category : #errors }
RBPragmaNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self arguments anySatisfy: [:each | each isFaulty]
]

{ #category : #testing }
RBPragmaNode >> isKeyword [
	^(selector indexOf: $:) ~= 0
]

{ #category : #testing }
RBPragmaNode >> isPragma [
	^ true
]

{ #category : #testing }
RBPragmaNode >> isPrimitive [
	^ #(primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:) includes: self selector
]

{ #category : #testing }
RBPragmaNode >> isUnary [
	^ arguments isEmpty
]

{ #category : #accessing }
RBPragmaNode >> keywordsIntervals [
	^selector keywords
		with: self keywordsPositions
		collect: [:keyword :start| start to: (start = 0 ifTrue: [ -1 ] ifFalse: [ start + keyword size - 1]) ]
]

{ #category : #accessing }
RBPragmaNode >> keywordsPositions [
	^keywordsPositions ifNil: [ (selector keywords collect: [:char| 0 ]) asIntegerArray ]
]

{ #category : #accessing }
RBPragmaNode >> keywordsPositions: aPositionsList [
	keywordsPositions := aPositionsList ifNotNil: [:list| list asIntegerArray ]
]

{ #category : #'accessing - token' }
RBPragmaNode >> left [
	^ left
]

{ #category : #'accessing - token' }
RBPragmaNode >> left: anInteger [
	left := anInteger
]

{ #category : #matching }
RBPragmaNode >> match: aNode inContext: aDictionary [
	aNode class = self class
		ifFalse: [ ^ false ].
	self selector = aNode selector
		ifFalse: [ ^ false ].
	self arguments with: aNode arguments do: [ :first :second |
		(first match: second inContext: aDictionary)
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #accessing }
RBPragmaNode >> numArgs [
	^ self selector numArgs
]

{ #category : #copying }
RBPragmaNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ])
]

{ #category : #replacing }
RBPragmaNode >> replaceNode: aNode withNode: anotherNode [
	self arguments: (arguments collect: [ :each |
		each == aNode
			ifTrue: [ anotherNode ]
			ifFalse: [ each ] ])
]

{ #category : #'accessing - token' }
RBPragmaNode >> right [
	^ right
]

{ #category : #'accessing - token' }
RBPragmaNode >> right: anInteger [
	right := anInteger
]

{ #category : #accessing }
RBPragmaNode >> selector [
	^ selector
]

{ #category : #accessing }
RBPragmaNode >> selector: aSelector [
	keywordsPositions := nil.
	selector := aSelector asSymbol
]

{ #category : #initialization }
RBPragmaNode >> selector: aSymbol keywordsPositions: positionList arguments: valueNodes [
	aSymbol numArgs == valueNodes size
		ifFalse:
			[self error: 'Attempting to assign selector with wrong number of arguments.'].

	self
		arguments: valueNodes;
		selector: aSymbol;
		keywordsPositions: positionList
]

{ #category : #accessing }
RBPragmaNode >> selectorParts [
	^ selector keywords collect: [:keyword| keyword asSymbol ]
]

{ #category : #accessing }
RBPragmaNode >> sentMessages [
	^ super sentMessages
		add: self selector;
		yourself
]

{ #category : #accessing }
RBPragmaNode >> start [
	^ left
]

{ #category : #accessing }
RBPragmaNode >> stop [
	^ right
]
