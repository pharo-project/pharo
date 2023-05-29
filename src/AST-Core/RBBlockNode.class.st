"
RBBlockNode is an AST node that represents a block ""[...]"".

Like RBMethodNode, the scope attribute is only valid after doing a semantic analyzing step.

Instance Variables:
	arguments	<SequenceableCollection of: RBVariableNode>	the arguments for the block
	bar	<Integer | nil>	position of the | after the arguments
	body	<RBSequenceNode>	the code inside the block
	colons	<SequenceableCollection of: Integer>	positions of each : before each argument
	left	<Integer>	position of [
	right	<Integer>	position of ]
	scope	<OCBlockScope | OCOptimizedBlockScope | nil> the scope associated with this code of this block
		
		
To get numbers of block usage in the system:

```
""Lots of Methods""
 Smalltalk globals methods size ""121357""

""there are quite some blocks""
allBlocks := Smalltalk globals methods flatCollect: [:meth | meth ast blockNodes ].
allBlocks size ""86028"". 

""but many are compiled inline (eg ifTrue:)""
currentFullBlocks := allBlocks select: [:blockNode | blockNode isInlined not].
currentFullBlocks size. ""36407""

""What we can compile as CleanBlockClosure""
cleanBlocks := currentFullBlocks select: [:blockNode | blockNode isClean].
cleanBlocks size. ""10097"" 

""the clean blocks are actually just constant""
constantBlocks := cleanBlocks select: [:blockNode | blockNode isConstant].
constantBlocks size. ""2540"" 

""FullBlocks that need the outerContext to return""
fullBocksWithReturn := currentFullBlocks select: [ :each  | each hasNonLocalReturn ].
fullBocksWithReturn size  ""2198""
```

"
Class {
	#name : #RBBlockNode,
	#superclass : #RBValueNode,
	#instVars : [
		'left',
		'right',
		'colons',
		'arguments',
		'bar',
		'body',
		'scope',
		'bcToASTCache'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBBlockNode class >> arguments: argNodes body: sequenceNode [
	^(self new)
		arguments: argNodes;
		body: sequenceNode;
		yourself
]

{ #category : #'instance creation' }
RBBlockNode class >> body: sequenceNode [
	^self arguments: #() body: sequenceNode
]

{ #category : #comparing }
RBBlockNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self body = anObject body ifFalse: [^false].
	self arguments size = anObject arguments size ifFalse: [^false].
	self arguments with: anObject arguments do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : #visiting }
RBBlockNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitBlockNode: self
]

{ #category : #accessing }
RBBlockNode >> allArgumentVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allArgumentVariables;
		yourself
]

{ #category : #accessing }
RBBlockNode >> allDefinedVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself
]

{ #category : #accessing }
RBBlockNode >> allStatements [
	"including temp variable definition."
	^ self temporaries asOrderedCollection copy
			addAll: super allStatements;
			yourself
]

{ #category : #accessing }
RBBlockNode >> argumentNames [
	^ self arguments collect: [:each | each name] as: Array
]

{ #category : #accessing }
RBBlockNode >> arguments [
	^arguments
]

{ #category : #accessing }
RBBlockNode >> arguments: argCollection [
	arguments := argCollection.
	arguments do: [:each | each parent: self ]
]

{ #category : #'accessing - token' }
RBBlockNode >> bar [
	^ bar
]

{ #category : #'accessing - token' }
RBBlockNode >> bar: anInteger [
	bar := anInteger
]

{ #category : #accessing }
RBBlockNode >> blockVariables [
	| vars |
	vars := super blockVariables asOrderedCollection.
	vars addAll: self argumentNames.
	^vars
]

{ #category : #accessing }
RBBlockNode >> body [
	^body
]

{ #category : #accessing }
RBBlockNode >> body: stmtsNode [
	body := stmtsNode.
	body parent: self
]

{ #category : #accessing }
RBBlockNode >> children [
	^ arguments copyWith: body
]

{ #category : #'accessing - token' }
RBBlockNode >> colons [
	^ colons
]

{ #category : #'accessing - token' }
RBBlockNode >> colons: anArray [
	colons := anArray
]

{ #category : #accessing }
RBBlockNode >> constantValue [
	self isConstant ifFalse: [ self error: 'block is not constant' ].
	^ body statements
		ifEmpty: [ nil ]
		ifNotEmpty: [:statements | statements first value ]
]

{ #category : #matching }
RBBlockNode >> copyInContext: aDictionary [
	^ self class new
		arguments: (self copyList: self arguments inContext: aDictionary);
		body: (self body copyInContext: aDictionary);
		yourself
]

{ #category : #testing }
RBBlockNode >> defines: aName [
	^arguments anySatisfy: [:each | each name = aName]
]

{ #category : #testing }
RBBlockNode >> directlyUses: aNode [
	^false
]

{ #category : #comparing }
RBBlockNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [^false].
	self arguments size = anObject arguments size ifFalse: [^false].
	self arguments
		with: anObject arguments
		do: [:first :second |	(first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	(self body equalTo: anObject body withMapping: aDictionary)
		ifFalse: [^false].
	self arguments do: [:each | aDictionary removeKey: each name].
	^true
]

{ #category : #testing }
RBBlockNode >> hasArgumentNamed: aString [
	^ self arguments anySatisfy: [ :argument| argument name = aString ]
]

{ #category : #testing }
RBBlockNode >> hasArguments [

	^ arguments isNotEmpty
]

{ #category : #testing }
RBBlockNode >> hasBlockReturn [
	^ self body lastIsReturn
]

{ #category : #testing }
RBBlockNode >> hasTemporaries [

	^ self temporaries isNotEmpty
]

{ #category : #testing }
RBBlockNode >> hasTemporaryNamed: aString [
	^ self temporaries anySatisfy: [ :temp| temp name = aString ]
]

{ #category : #comparing }
RBBlockNode >> hash [
	^ (self hashForCollection: self arguments) bitXor: self body hash
]

{ #category : #testing }
RBBlockNode >> headIsNotEmpty [
	"Return true whether the receiver has some arguments or comments"

	^ arguments isNotEmpty or: [ self comments isNotEmpty ]
]

{ #category : #initialization }
RBBlockNode >> initialize [
	super initialize.

	arguments := #().
	colons := #().
	left := 0.
	right := 0
]

{ #category : #testing }
RBBlockNode >> isBlock [

	^ true
]

{ #category : #testing }
RBBlockNode >> isConstant [
	"is the block just returning a literal?"
	^ body statements
		ifEmpty: [ true "empty block returns nil" ]
		ifNotEmpty: [:statements | statements first isLiteralNode ]
]

{ #category : #errors }
RBBlockNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^(self arguments anySatisfy: [:each | each isFaulty] ) or: [ self body isFaulty]
]

{ #category : #testing }
RBBlockNode >> isImmediateNode [
	^true
]

{ #category : #testing }
RBBlockNode >> isLast: aNode [
	^body isLast: aNode
]

{ #category : #testing }
RBBlockNode >> lastIsReturn [
	^ false
]

{ #category : #'accessing - token' }
RBBlockNode >> left [
	^ left
]

{ #category : #'accessing - token' }
RBBlockNode >> left: anInteger [
	left := anInteger
]

{ #category : #matching }
RBBlockNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^(self
		matchList: arguments
		against: aNode arguments
		inContext: aDictionary)
			and: [body match: aNode body inContext: aDictionary]
]

{ #category : #accessing }
RBBlockNode >> methodOrBlockNode [
	^ self
]

{ #category : #testing }
RBBlockNode >> needsParenthesis [
	^false
]

{ #category : #accessing }
RBBlockNode >> numArgs [
	^ self arguments size
]

{ #category : #copying }
RBBlockNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ]).
	self body: self body copy
]

{ #category : #accessing }
RBBlockNode >> precedence [
	^0
]

{ #category : #testing }
RBBlockNode >> references: aVariableName [
	^body references: aVariableName
]

{ #category : #'adding-removing' }
RBBlockNode >> removeTemporaryNamed: aName [

	self body removeTemporaryNamed: aName
]

{ #category : #replacing }
RBBlockNode >> replaceNode: aNode withNode: anotherNode [
	body == aNode ifTrue: [self body: anotherNode].
	self arguments: (arguments
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : #'accessing - token' }
RBBlockNode >> right [
	^ right
]

{ #category : #'accessing - token' }
RBBlockNode >> right: anInteger [
	right := anInteger
]

{ #category : #accessing }
RBBlockNode >> scope [
	^ scope
]

{ #category : #accessing }
RBBlockNode >> scope: aScopedNode [
	scope := aScopedNode
]

{ #category : #accessing }
RBBlockNode >> startWithoutParentheses [
	^ left
]

{ #category : #accessing }
RBBlockNode >> statementComments [
	"this is used for formatting comments for this node as a statement, but I don't have a single statement comment, my comments are place within the block code"

	^ ''
]

{ #category : #accessing }
RBBlockNode >> statements [
	^ self body statements
]

{ #category : #accessing }
RBBlockNode >> stopWithoutParentheses [
	^ right
]

{ #category : #accessing }
RBBlockNode >> temporaries [
	^ self body temporaries
]

{ #category : #accessing }
RBBlockNode >> temporaryNames [
	^ self body temporaryNames
]

{ #category : #testing }
RBBlockNode >> uses: aNode [
	aNode = body ifFalse: [^false].
	^parent isMessage
		ifTrue:
			[(#(#ifTrue:ifFalse: #ifTrue: #ifFalse: #ifFalse:ifTrue:)
				includes: parent selector) not
				or: [parent isUsed]]
		ifFalse: [self isUsed]
]
