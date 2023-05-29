"
An RBArrayNode is an AST node for runtime arrays.

Instance Variables
	left:	 <Integer | nil> position of {
	periods: <SequenceableCollection of: Integer> the positions of all the periods that separate the statements
	right: <Integer | nil> position of }
	statements: <SequenceableCollection of: RBValueNode> the statement nodes
"
Class {
	#name : #RBArrayNode,
	#superclass : #RBValueNode,
	#instVars : [
		'left',
		'right',
		'statements',
		'periods'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBArrayNode class >> statements: statements [
	^ self new statements: statements
]

{ #category : #comparing }
RBArrayNode >> = anObject [
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	self statements size = anObject statements size ifFalse: [ ^ false ].
	self statements with: anObject statements do: [ :first :second | first = second ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #visiting }
RBArrayNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitArrayNode: self
]

{ #category : #'adding nodes' }
RBArrayNode >> addNode: aNode [
	statements := statements asOrderedCollection
		add: aNode;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : #'adding nodes' }
RBArrayNode >> addNode: aNode before: anotherNode [
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

{ #category : #'adding nodes' }
RBArrayNode >> addNodeFirst: aNode [
	statements := statements asOrderedCollection
		addFirst: aNode;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : #'adding nodes' }
RBArrayNode >> addNodes: aCollection [
	statements := statements asOrderedCollection
		addAll: aCollection;
		yourself.
	aCollection do: [ :each | each parent: self ].
	^ aCollection
]

{ #category : #'adding nodes' }
RBArrayNode >> addNodes: aCollection before: anotherNode [
	aCollection do: [ :each | self addNode: each before: anotherNode ].
	^ aCollection
]

{ #category : #'adding nodes' }
RBArrayNode >> addNodesFirst: aCollection [
	statements := statements asOrderedCollection
		addAllFirst: aCollection;
		yourself.
	aCollection do: [ :each | each parent: self ].
	^ aCollection
]

{ #category : #accessing }
RBArrayNode >> allStatements [
	^ statements asOrderedCollection copy
			addAll: super allStatements;
			yourself
]

{ #category : #accessing }
RBArrayNode >> children [
	^ statements
]

{ #category : #matching }
RBArrayNode >> copyInContext: aDictionary [
	^ self class statements: (self copyList: self statements inContext: aDictionary)
]

{ #category : #comparing }
RBArrayNode >> equalTo: anObject withMapping: aDictionary [
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

{ #category : #comparing }
RBArrayNode >> hash [
	^ self hashForCollection: self statements
]

{ #category : #private }
RBArrayNode >> indexOfNode: aNode [
	"Try to find the node by first looking for ==, and then for ="

	^ (1 to: statements size)
		detect: [ :each | (statements at: each) == aNode ]
		ifNone: [ statements indexOf: aNode ]
]

{ #category : #initialization }
RBArrayNode >> initialize [
	super initialize.
	statements := OrderedCollection new.
	periods := #().
	left := right := 0
]

{ #category : #testing }
RBArrayNode >> isDynamicArray [
	^ true
]

{ #category : #testing }
RBArrayNode >> isEmpty [
	^ statements isEmpty
]

{ #category : #errors }
RBArrayNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self statements anySatisfy: [:each | each isFaulty]
]

{ #category : #testing }
RBArrayNode >> lastIsReturn [
	statements isEmpty ifTrue:[ ^false ].
	^statements last lastIsReturn
]

{ #category : #'accessing - token' }
RBArrayNode >> left [
	^ left
]

{ #category : #'accessing - token' }
RBArrayNode >> left: anInteger [
	left := anInteger
]

{ #category : #matching }
RBArrayNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [ ^ false ].
	^ self matchList: statements against: aNode statements inContext: aDictionary
]

{ #category : #testing }
RBArrayNode >> needsParenthesis [
	^ false
]

{ #category : #'accessing - token' }
RBArrayNode >> periods [
	^ periods
]

{ #category : #'accessing - token' }
RBArrayNode >> periods: anArray [
	periods := anArray
]

{ #category : #copying }
RBArrayNode >> postCopy [
	super postCopy.
	self statements: (self statements collect: [ :each | each copy ])
]

{ #category : #accessing }
RBArrayNode >> precedence [
	^0
]

{ #category : #testing }
RBArrayNode >> references: aVariableName [
	^ statements anySatisfy: [ :each | each references: aVariableName ]
]

{ #category : #replacing }
RBArrayNode >> replaceNode: oldNode  withNode: newNode [
	self statements: (statements collect: [ :statement |
		statement == oldNode
			ifTrue: [ newNode ]
			ifFalse: [ statement ] ])
]

{ #category : #'accessing - token' }
RBArrayNode >> right [
	^ right
]

{ #category : #'accessing - token' }
RBArrayNode >> right: anInteger [
	right := anInteger
]

{ #category : #accessing }
RBArrayNode >> startWithoutParentheses [
	^ left
]

{ #category : #accessing }
RBArrayNode >> statementComments [
	^self comments
]

{ #category : #accessing }
RBArrayNode >> statements [
	^statements
]

{ #category : #accessing }
RBArrayNode >> statements: statements0 [
	statements := statements0.
	statements do: [:statement | statement parent: self]
]

{ #category : #accessing }
RBArrayNode >> stopWithoutParentheses [
	^ right
]

{ #category : #testing }
RBArrayNode >> uses: aNode [
	^ (statements anySatisfy: [ :each | each == aNode ]) or: [ self isUsed ]
]
