"
An ASTArrayNode is an AST node for runtime arrays.

Instance Variables
	left:	 <Integer | nil> position of {
	periods: <SequenceableCollection of: Integer> the positions of all the periods that separate the statements
	right: <Integer | nil> position of }
	statements: <SequenceableCollection of: ASTValueNode> the statement nodes
"
Class {
	#name : 'OCArrayNode',
	#superclass : 'OCValueNode',
	#instVars : [
		'left',
		'right',
		'statements',
		'periods'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCArrayNode class >> statements: statements [
	^ self new statements: statements
]

{ #category : 'comparing' }
OCArrayNode >> = anObject [
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	self statements size = anObject statements size ifFalse: [ ^ false ].
	self statements with: anObject statements do: [ :first :second | first = second ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'visiting' }
OCArrayNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitArrayNode: self
]

{ #category : 'adding nodes' }
OCArrayNode >> addNode: aNode [
	statements := statements asOrderedCollection
		add: aNode;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : 'adding nodes' }
OCArrayNode >> addNode: aNode before: anotherNode [
	| index |
	aNode isReturn
		ifTrue: [ self error: 'Cannot add return node' ].
	index := self indexOfNode: anotherNode.
	index = 0 ifTrue: [ ^ self addNode: aNode ].
	statements := statements asOrderedCollection
		add: aNode beforeIndex: index;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : 'adding nodes' }
OCArrayNode >> addNodeFirst: aNode [
	statements := statements asOrderedCollection
		addFirst: aNode;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : 'adding nodes' }
OCArrayNode >> addNodes: aCollection [
	statements := statements asOrderedCollection
		addAll: aCollection;
		yourself.
	aCollection do: [ :each | each parent: self ].
	^ aCollection
]

{ #category : 'adding nodes' }
OCArrayNode >> addNodes: aCollection before: anotherNode [
	aCollection do: [ :each | self addNode: each before: anotherNode ].
	^ aCollection
]

{ #category : 'adding nodes' }
OCArrayNode >> addNodesFirst: aCollection [
	statements := statements asOrderedCollection
		addAllFirst: aCollection;
		yourself.
	aCollection do: [ :each | each parent: self ].
	^ aCollection
]

{ #category : 'accessing' }
OCArrayNode >> allStatements [

	^ (OrderedCollection withAll: statements)
		  addAll: super allStatements;
		  yourself
]

{ #category : 'accessing' }
OCArrayNode >> children [
	^ statements
]

{ #category : 'matching' }
OCArrayNode >> copyInContext: aDictionary [
	^ self class statements: (self copyList: self statements inContext: aDictionary)
]

{ #category : 'comparing' }
OCArrayNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class
		ifFalse: [ ^ false ].
	self statements size = anObject statements size
		ifFalse: [ ^ false ].
	self statements with: anObject statements do: [ :first :second |
		(first equalTo: second withMapping: aDictionary)
			ifFalse: [ ^ false ] ].
	aDictionary values asSet size = aDictionary size
		ifFalse: [ ^ false ].
	^ true
]

{ #category : 'testing' }
OCArrayNode >> hasBlock [

	^ statements anySatisfy: [ :stm | stm hasBlock ]
]

{ #category : 'comparing' }
OCArrayNode >> hash [
	^ self hashForCollection: self statements
]

{ #category : 'private' }
OCArrayNode >> indexOfNode: aNode [
	"Try to find the node by first looking for ==, and then for ="

	^ (1 to: statements size)
		detect: [ :each | (statements at: each) == aNode ]
		ifNone: [ statements indexOf: aNode ]
]

{ #category : 'initialization' }
OCArrayNode >> initialize [
	super initialize.
	statements := OrderedCollection new.
	periods := #().
	left := right := 0
]

{ #category : 'testing' }
OCArrayNode >> isDynamicArray [
	^ true
]

{ #category : 'testing' }
OCArrayNode >> isEmpty [
	^ statements isEmpty
]

{ #category : 'errors' }
OCArrayNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self statements anySatisfy: [:each | each isFaulty]
]

{ #category : 'testing' }
OCArrayNode >> isUsingAsReturnValue: aNode [
	^ (statements anySatisfy: [ :each | each == aNode ]) or: [ self isUsedAsReturnValue ]
]

{ #category : 'testing' }
OCArrayNode >> lastIsReturn [

	^ statements isEmpty
		  ifTrue: [ false ]
		  ifFalse: [ statements last lastIsReturn ]
]

{ #category : 'accessing - token' }
OCArrayNode >> left [
	^ left
]

{ #category : 'accessing - token' }
OCArrayNode >> left: anInteger [
	left := anInteger
]

{ #category : 'matching' }
OCArrayNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [ ^ false ].
	^ self matchList: statements against: aNode statements inContext: aDictionary
]

{ #category : 'testing' }
OCArrayNode >> needsParenthesis [
	^ false
]

{ #category : 'accessing - token' }
OCArrayNode >> periods [
	^ periods
]

{ #category : 'accessing - token' }
OCArrayNode >> periods: anArray [
	periods := anArray
]

{ #category : 'copying' }
OCArrayNode >> postCopy [
	super postCopy.
	self statements: (self statements collect: [ :each | each copy ])
]

{ #category : 'accessing' }
OCArrayNode >> precedence [
	^0
]

{ #category : 'testing' }
OCArrayNode >> references: aVariableName [
	^ statements anySatisfy: [ :each | each references: aVariableName ]
]

{ #category : 'replacing' }
OCArrayNode >> replaceNode: oldNode  withNode: newNode [
	self statements: (statements collect: [ :statement |
		statement == oldNode
			ifTrue: [ newNode ]
			ifFalse: [ statement ] ])
]

{ #category : 'accessing - token' }
OCArrayNode >> right [
	^ right
]

{ #category : 'accessing - token' }
OCArrayNode >> right: anInteger [
	right := anInteger
]

{ #category : 'accessing' }
OCArrayNode >> startWithoutParentheses [
	^ left
]

{ #category : 'accessing' }
OCArrayNode >> statementComments [
	^self comments
]

{ #category : 'accessing' }
OCArrayNode >> statements [
	^statements
]

{ #category : 'accessing' }
OCArrayNode >> statements: statements0 [
	statements := statements0.
	statements do: [:statement | statement parent: self]
]

{ #category : 'accessing' }
OCArrayNode >> stopWithoutParentheses [
	^ right
]
