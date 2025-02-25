"
ASTSequenceNode is an AST node that represents a sequence of statements. Both ASTBlockNodes and ASTMethodNodes contain these.

Instance Variables:
	leftBar	<Integer | nil>	the position of the left | in the temporaries definition
	rightBar	<Integer | nil>	the position of the right | in the temporaries definition
	statements	<SequenceableCollection of: ASTReturnNode or ASTValueNode> the statement nodes
	periods	<SequenceableCollection of: Integer>	the positions of all the periods that separate the statements
	temporaries	<SequenceableCollection of: ASTVariableNode>	the temporaries defined


"
Class {
	#name : 'OCSequenceNode',
	#superclass : 'OCProgramNode',
	#instVars : [
		'leftBar',
		'rightBar',
		'statements',
		'periods',
		'temporaries'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCSequenceNode class >> leftBar: leftInteger temporaries: variableNodes rightBar: rightInteger [
	^(self new)
		leftBar: leftInteger
			temporaries: variableNodes
			rightBar: rightInteger;
		yourself
]

{ #category : 'instance creation' }
OCSequenceNode class >> statements: statementNodes [
	^self temporaries: #() statements: statementNodes
]

{ #category : 'instance creation' }
OCSequenceNode class >> temporaries: variableNodes statements: statementNodes [
	^(self new)
		temporaries: variableNodes;
		statements: statementNodes;
		yourself
]

{ #category : 'comparing' }
OCSequenceNode >> = anObject [
	"Can't send = to the temporaries and statements collection since they might change from arrays to OCs"

	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self temporaries size = anObject temporaries size ifFalse: [^false].
	self temporaries
		with: anObject temporaries
		do: [:first :second | first = second ifFalse: [^false]].
	self statements size = anObject statements size ifFalse: [^false].
	self statements
		with: anObject statements
		do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : 'visiting' }
OCSequenceNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSequenceNode: self
]

{ #category : 'adding nodes' }
OCSequenceNode >> addFaultyNode: aNode [
"just like addNode, but ignore if we already include a return node, as we are faulty"
	aNode parent: self.
	statements := statements asOrderedCollection
		add: aNode;
		yourself.
	^ aNode
]

{ #category : 'adding nodes' }
OCSequenceNode >> addNode: aNode [
	aNode parent: self.
	(statements notEmpty and: [ statements last isReturn ])
		ifTrue: [ self error: 'Cannot add statement after return node' ].
	statements := statements asOrderedCollection
		add: aNode;
		yourself.
	^ aNode
]

{ #category : 'adding nodes' }
OCSequenceNode >> addNode: aNode after: anotherNode [

	| index |
	index := self indexOfNode: anotherNode.
	index = 0
		ifTrue: [ ^ self addNode: aNode ].
	statements := statements asOrderedCollection
		add: aNode afterIndex: index;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : 'adding nodes' }
OCSequenceNode >> addNode: aNode before: anotherNode [
	| index |
	index := self indexOfNode: anotherNode.
	index = 0
		ifTrue: [ ^ self addNode: aNode ].
	statements := statements asOrderedCollection
		add: aNode beforeIndex: index;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : 'adding nodes' }
OCSequenceNode >> addNodeFirst: aNode [
	aNode parent: self.
	statements := statements asOrderedCollection
		addFirst: aNode;
		yourself.
	^ aNode
]

{ #category : 'adding nodes' }
OCSequenceNode >> addNodeLast: aNode [

	self lastIsReturn
		ifTrue: [ self addNode: aNode before: self statements last ]
		ifFalse: [ self addNode: aNode ]
]

{ #category : 'adding nodes' }
OCSequenceNode >> addNodes: aCollection [
	aCollection do: [ :each | each parent: self ].
	(statements notEmpty and: [ statements last isReturn ])
		ifTrue: [ self error: 'Cannot add statement after return node' ].
	statements := statements asOrderedCollection
		addAll: aCollection;
		yourself.
	^ aCollection
]

{ #category : 'adding nodes' }
OCSequenceNode >> addNodes: aCollection before: anotherNode [
	aCollection do: [ :each | self addNode: each before: anotherNode ].
	^ aCollection
]

{ #category : 'adding nodes' }
OCSequenceNode >> addNodesFirst: aCollection [
	aCollection do: [ :each | each parent: self ].
	statements := statements asOrderedCollection
		addAllFirst: aCollection;
		yourself.
	^ aCollection
]

{ #category : 'adding nodes' }
OCSequenceNode >> addReturn [
	| node |

	statements isEmpty
		ifTrue: [ ^ nil ].
	statements last isReturn
		ifTrue: [ ^ statements last ].
	node := OCReturnNode start: statements last start value: statements last.
	statements at: statements size put: node.
	node parent: self.

]

{ #category : 'adding nodes' }
OCSequenceNode >> addSelfReturn [
	| node |
	self lastIsReturn
		ifTrue: [ ^ self statements last ].
	node := OCReturnNode value: OCVariableNode selfNode.
	^ self addNode: node
]

{ #category : 'adding nodes' }
OCSequenceNode >> addTemporariesNamed: aCollection [
	^ aCollection collect: [ :each | self addTemporaryNamed: each ]
]

{ #category : 'adding nodes' }
OCSequenceNode >> addTemporaryNamed: aString [
	| variableNode |
	variableNode := OCVariableNode named: aString.
	variableNode parent: self.
	temporaries := temporaries copyWith: variableNode.
	^ variableNode
]

{ #category : 'accessing' }
OCSequenceNode >> allDefinedVariables [
	^(self temporaryNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself
]

{ #category : 'accessing' }
OCSequenceNode >> allStatements [

	^ (OrderedCollection withAll: statements)
		  addAll: super allStatements;
		  yourself
]

{ #category : 'accessing' }
OCSequenceNode >> allTemporaryVariables [
	^(self temporaryNames asOrderedCollection)
		addAll: super allTemporaryVariables;
		yourself
]

{ #category : 'converting' }
OCSequenceNode >> asSequenceNode [
	^self
]

{ #category : 'querying' }
OCSequenceNode >> bestNodeFor: anInterval [

	| node intersectingStatements |
	" if the interval contains myself precisely, return self "
	(self start == anInterval first and: [ self stop == anInterval last ])
		ifTrue: [ ^ self ].

	intersectingStatements := self statementsIntersectingInterval:
		                          anInterval.
	" if the interval intersects more than one of my statements, return self "
	intersectingStatements size > 1 ifTrue: [ ^ self ].

	intersectingStatements do: [ :statement |
		^ (statement intersectsInterval: anInterval)
			ifTrue: [ statement bestNodeFor: anInterval ]
			ifFalse: [ statement ] ]. "anInterval intersects the statement's final period"

	node := super bestNodeFor: anInterval.
	node == self ifTrue: [
		(temporaries isEmpty and: [ statements size == 1 ]) 
			ifTrue: [ ^ statements first ] ].
	^ node
]

{ #category : 'accessing' }
OCSequenceNode >> children [
	^(OrderedCollection new)
		addAll: temporaries;
		addAll: statements;
		yourself
]

{ #category : 'matching' }
OCSequenceNode >> copyInContext: aDictionary [
	^ self class new
		temporaries: (self copyList: self temporaries inContext: aDictionary);
		statements: (self copyList: self statements inContext: aDictionary);
		yourself
]

{ #category : 'testing' }
OCSequenceNode >> defines: aName [
	^temporaries anySatisfy: [:each | each name = aName]
]

{ #category : 'comparing' }
OCSequenceNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [^false].
	self statements size = anObject statements size ifFalse: [^false].
	self statements
		with: anObject statements
		do: [:first :second | (first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	aDictionary values asSet size = aDictionary size ifFalse: [^false].	"Not a one-to-one mapping"
	self temporaries
		do: [:each | aDictionary removeKey: each name ifAbsent: []].
	^true
]

{ #category : 'testing' }
OCSequenceNode >> hasBlock [

	^ statements anySatisfy: [ :stm | stm hasBlock ]
]

{ #category : 'testing' }
OCSequenceNode >> hasNonLocalReturn [
	"check if there is a non-local return anywhere
	Note: returns in a method itself are local returns"
	^ self lastIsReturn
		  ifTrue: [ parent isMethod not ]
		  ifFalse: [ super hasNonLocalReturn ]
]

{ #category : 'testing' }
OCSequenceNode >> hasSameExitPoint [

	statements last isReturn ifTrue: [ ^ true ].
	^ self hasSameExitPoint: false
]

{ #category : 'testing' }
OCSequenceNode >> hasSameExitPoint: aBoolean [ 

	| statementsWithBlocks |
	aBoolean ifTrue: [ ^ true ].
	statements ifEmpty: [ ^ true ].
	statementsWithBlocks := statements select: [ :stm | stm hasBlock ].
	^ (statements last isReturn not and: [
		statementsWithBlocks allSatisfy: [ :stm | stm hasSameExitPoint: false ] ]).
]

{ #category : 'testing' }
OCSequenceNode >> hasTemporaries [

	^ temporaries isNotEmpty
]

{ #category : 'testing' }
OCSequenceNode >> hasTemporaryNamed: aString [

	^ temporaries anySatisfy: [ :temp | temp name = aString ]
]

{ #category : 'comparing' }
OCSequenceNode >> hash [
	^ (self hashForCollection: self temporaries) bitXor: (self hashForCollection: self statements)
]

{ #category : 'private' }
OCSequenceNode >> indexOfNode: aNode [
	"Try to find the node by first looking for ==, and then for ="

	^(1 to: statements size) detect: [:each | (statements at: each) == aNode]
		ifNone: [statements indexOf: aNode]
]

{ #category : 'initialization' }
OCSequenceNode >> initialize [

	super initialize.
	periods := OrderedCollection new.
	statements := OrderedCollection new.
	temporaries := OrderedCollection new
]

{ #category : 'testing' }
OCSequenceNode >> isEssentialChild: aNode [
	^false
]

{ #category : 'errors' }
OCSequenceNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	(self temporaries anySatisfy: [:each | each isFaulty]) ifTrue:[ ^true ].
	^self statements anySatisfy: [:each | each isFaulty]
]

{ #category : 'testing' }
OCSequenceNode >> isLast: aNode [
	| last |
	statements isEmpty ifTrue: [^false].
	last := statements last.
	^last == aNode or:
			[last isMessage and:
					[(#(#ifTrue:ifFalse: #ifFalse:ifTrue:) includes: last selector)
						and: [last arguments anySatisfy: [:each | each isLast: aNode]]]]
]

{ #category : 'testing' }
OCSequenceNode >> isSequence [
	^true
]

{ #category : 'testing' }
OCSequenceNode >> isUsingAsReturnValue: aNode [
	statements isEmpty ifTrue: [^false].
	aNode == statements last ifFalse: [^false].
	"We will come to this line only if the `aNode` is the last node in the sequence.
	If the parent is block we return true since it's return value of the block and it cannot be removed."
	"self parent isBlock ifTrue: [ ^ true ]."
	"Otherwise, we let the parent decide."
	^self isUsedAsReturnValue
]

{ #category : 'testing' }
OCSequenceNode >> lastIsReturn [
	^statements notEmpty and: [statements last lastIsReturn]
]

{ #category : 'accessing - token' }
OCSequenceNode >> leftBar [
	^ leftBar
]

{ #category : 'accessing - token' }
OCSequenceNode >> leftBar: anInteger [
	leftBar := anInteger
]

{ #category : 'initialization' }
OCSequenceNode >> leftBar: leftInteger temporaries: variableNodes rightBar: rightInteger [
	leftBar := leftInteger.
	self temporaries: variableNodes.
	rightBar := rightInteger
]

{ #category : 'matching' }
OCSequenceNode >> match: aNode inContext: aDictionary [
	self class = aNode class ifFalse: [^false].
	^(self
		matchList: temporaries
		against: aNode temporaries
		inContext: aDictionary) and:
				[self
					matchList: statements
					against: aNode statements
					inContext: aDictionary]
]

{ #category : 'accessing - token' }
OCSequenceNode >> periods [
	^ periods
]

{ #category : 'accessing - token' }
OCSequenceNode >> periods: anArray [
	periods := anArray
]

{ #category : 'copying' }
OCSequenceNode >> postCopy [
	super postCopy.
	self temporaries: (self temporaries collect: [ :each | each copy ]).
	self statements: (self statements collect: [ :each | each copy ])
]

{ #category : 'testing' }
OCSequenceNode >> references: aVariableName [
	^statements anySatisfy: [:each | each references: aVariableName]
]

{ #category : 'replacing' }
OCSequenceNode >> removeDeadCode [
	(self isUsedAsReturnValue ifTrue: [statements size - 1] ifFalse: [statements size])
		to: 1
		by: -1
		do:
			[:i |
			(statements at: i) isImmediateNode
				ifTrue:
					[self clearReplacements.
					statements removeAt: i]].
	super removeDeadCode
]

{ #category : 'replacing' }
OCSequenceNode >> removeNode: aNode [
	self replaceNode: aNode withNodes: #()
]

{ #category : 'accessing' }
OCSequenceNode >> removeTemporaryNamed: aName [
	temporaries := temporaries reject: [:each | each name = aName]
]

{ #category : 'replacing' }
OCSequenceNode >> replaceNode: aNode withNode: anotherNode [
	self statements: (statements
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]]).
	self temporaries: (temporaries
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'replacing' }
OCSequenceNode >> replaceNode: aNode withNodes: aCollection [
	| index newStatements |
	self clearReplacements.
	index := self indexOfNode: aNode.
	newStatements := OrderedCollection new: statements size + aCollection size.
	1 to: index - 1 do: [:i | newStatements add: (statements at: i)].
	newStatements addAll: aCollection.
	index + 1 to: statements size
		do: [:i | newStatements add: (statements at: i)].
	aCollection do: [:each | each parent: self].
	statements := newStatements
]

{ #category : 'accessing - token' }
OCSequenceNode >> rightBar [
	^ rightBar
]

{ #category : 'accessing - token' }
OCSequenceNode >> rightBar: anInteger [
	rightBar := anInteger
]

{ #category : 'accessing' }
OCSequenceNode >> start [
	^ leftBar
		ifNil: [statements isEmpty ifTrue: [1] ifFalse: [statements first start]]
]

{ #category : 'accessing' }
OCSequenceNode >> statements [
	^statements
]

{ #category : 'accessing' }
OCSequenceNode >> statements: stmtCollection [
	statements := stmtCollection.
	statements do: [:each | each parent: self]
]

{ #category : 'querying' }
OCSequenceNode >> statementsIntersectingInterval: anInterval [

	| found |
	found := #(  ) asOrderedCollection.

	" search for statements intersecting anInterval, consider final periods as part of the statement "
	statements doWithIndex: [ :statement :index |
		((statement intersectsInterval: anInterval) or: [
			 (periods at: index ifAbsent: [ 0 ]) == anInterval first ])
			ifTrue: [ found add: statement ] ].
	^ found
]

{ #category : 'accessing' }
OCSequenceNode >> stop [
	^{
	temporaries isEmpty ifTrue: [0] ifFalse: [ self temporaries last stop].
	periods isEmpty ifTrue: [0] ifFalse: [periods last].
	statements isEmpty ifTrue: [0] ifFalse: [statements last stop]
	} max
]

{ #category : 'accessing' }
OCSequenceNode >> temporaries [
	^temporaries
]

{ #category : 'accessing' }
OCSequenceNode >> temporaries: tempCollection [
	temporaries := tempCollection.
	temporaries do: [:each | each parent: self]
]

{ #category : 'accessing' }
OCSequenceNode >> temporaryNames [
	^temporaries collect: [:each | each name]
]

{ #category : 'accessing' }
OCSequenceNode >> temporaryVariables [
	^(super temporaryVariables asOrderedCollection)
		addAll: self temporaryNames;
		yourself
]

{ #category : 'adding nodes' }
OCSequenceNode >> transformLastToReturn [
	"transform the last statement, but this method returns the sequence, not the returnNode"
	self addReturn
]

{ #category : 'querying' }
OCSequenceNode >> whichNodeIsContainedBy: anInterval [

	| node |
	node := super whichNodeIsContainedBy: anInterval.
	node == self
		ifTrue: [(temporaries isEmpty and: [statements size == 1])
						ifTrue: [ ^ statements first ] ].
	^ node
]
