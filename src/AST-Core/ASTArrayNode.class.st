"
An ASTArrayNode is an AST node for runtime arrays.

Instance Variables
	left:	 <Integer | nil> position of {
	periods: <SequenceableCollection of: Integer> the positions of all the periods that separate the statements
	right: <Integer | nil> position of }
	statements: <SequenceableCollection of: ASTValueNode> the statement nodes
"
Class {
	#name : 'ASTArrayNode',
	#superclass : 'ASTValueNode',
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
ASTArrayNode class >> statements: statements [
	^ self new statements: statements
]

{ #category : 'comparing' }
ASTArrayNode >> = anObject [
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	self statements size = anObject statements size ifFalse: [ ^ false ].
	self statements with: anObject statements do: [ :first :second | first = second ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'visiting' }
ASTArrayNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitArrayNode: self
]

{ #category : 'adding nodes' }
ASTArrayNode >> addNode: aNode [
	statements := statements asOrderedCollection
		add: aNode;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : 'adding nodes' }
ASTArrayNode >> addNode: aNode before: anotherNode [
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
ASTArrayNode >> addNodeFirst: aNode [
	statements := statements asOrderedCollection
		addFirst: aNode;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : 'adding nodes' }
ASTArrayNode >> addNodes: aCollection [
	statements := statements asOrderedCollection
		addAll: aCollection;
		yourself.
	aCollection do: [ :each | each parent: self ].
	^ aCollection
]

{ #category : 'adding nodes' }
ASTArrayNode >> addNodes: aCollection before: anotherNode [
	aCollection do: [ :each | self addNode: each before: anotherNode ].
	^ aCollection
]

{ #category : 'adding nodes' }
ASTArrayNode >> addNodesFirst: aCollection [
	statements := statements asOrderedCollection
		addAllFirst: aCollection;
		yourself.
	aCollection do: [ :each | each parent: self ].
	^ aCollection
]

{ #category : 'accessing' }
ASTArrayNode >> allStatements [

	^ (OrderedCollection withAll: statements)
		  addAll: super allStatements;
		  yourself
]

{ #category : 'accessing' }
ASTArrayNode >> children [
	^ statements
]

{ #category : 'matching' }
ASTArrayNode >> copyInContext: aDictionary [
	^ self class statements: (self copyList: self statements inContext: aDictionary)
]

{ #category : 'comparing' }
ASTArrayNode >> equalTo: anObject withMapping: aDictionary [
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
ASTArrayNode >> hasBlock [

	^ statements anySatisfy: [ :stm | stm hasBlock ]
]

{ #category : 'comparing' }
ASTArrayNode >> hash [
	^ self hashForCollection: self statements
]

{ #category : 'private' }
ASTArrayNode >> indexOfNode: aNode [
	"Try to find the node by first looking for ==, and then for ="

	^ (1 to: statements size)
		detect: [ :each | (statements at: each) == aNode ]
		ifNone: [ statements indexOf: aNode ]
]

{ #category : 'initialization' }
ASTArrayNode >> initialize [
	super initialize.
	statements := OrderedCollection new.
	periods := #().
	left := right := 0
]

{ #category : 'testing' }
ASTArrayNode >> isDynamicArray [
	^ true
]

{ #category : 'testing' }
ASTArrayNode >> isEmpty [
	^ statements isEmpty
]

{ #category : 'errors' }
ASTArrayNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self statements anySatisfy: [:each | each isFaulty]
]

{ #category : 'testing' }
ASTArrayNode >> isUsingAsReturnValue: aNode [
	^ (statements anySatisfy: [ :each | each == aNode ]) or: [ self isUsedAsReturnValue ]
]

{ #category : 'testing' }
ASTArrayNode >> lastIsReturn [

	^ statements isEmpty
		  ifTrue: [ false ]
		  ifFalse: [ statements last lastIsReturn ]
]

{ #category : 'accessing - token' }
ASTArrayNode >> left [
	^ left
]

{ #category : 'accessing - token' }
ASTArrayNode >> left: anInteger [
	left := anInteger
]

{ #category : 'matching' }
ASTArrayNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [ ^ false ].
	^ self matchList: statements against: aNode statements inContext: aDictionary
]

{ #category : 'testing' }
ASTArrayNode >> needsParenthesis [
	^ false
]

{ #category : 'accessing - token' }
ASTArrayNode >> periods [
	^ periods
]

{ #category : 'accessing - token' }
ASTArrayNode >> periods: anArray [
	periods := anArray
]

{ #category : 'copying' }
ASTArrayNode >> postCopy [
	super postCopy.
	self statements: (self statements collect: [ :each | each copy ])
]

{ #category : 'accessing' }
ASTArrayNode >> precedence [
	^0
]

{ #category : 'testing' }
ASTArrayNode >> references: aVariableName [
	^ statements anySatisfy: [ :each | each references: aVariableName ]
]

{ #category : 'replacing' }
ASTArrayNode >> replaceNode: oldNode  withNode: newNode [
	self statements: (statements collect: [ :statement |
		statement == oldNode
			ifTrue: [ newNode ]
			ifFalse: [ statement ] ])
]

{ #category : 'accessing - token' }
ASTArrayNode >> right [
	^ right
]

{ #category : 'accessing - token' }
ASTArrayNode >> right: anInteger [
	right := anInteger
]

{ #category : 'accessing' }
ASTArrayNode >> startWithoutParentheses [
	^ left
]

{ #category : 'accessing' }
ASTArrayNode >> statementComments [
	^self comments
]

{ #category : 'accessing' }
ASTArrayNode >> statements [
	^statements
]

{ #category : 'accessing' }
ASTArrayNode >> statements: statements0 [
	statements := statements0.
	statements do: [:statement | statement parent: self]
]

{ #category : 'accessing' }
ASTArrayNode >> stopWithoutParentheses [
	^ right
]
