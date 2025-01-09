"
ASTBlockNode is an AST node that represents a block ""[...]"".

Like ASTMethodNode, the scope attribute is only valid after doing a semantic analyzing step.

Instance Variables:
	arguments	<SequenceableCollection of: ASTVariableNode>	the arguments for the block
	bar	<Integer | nil>	position of the | after the arguments
	body	<ASTSequenceNode>	the code inside the block
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
	#name : 'OCBlockNode',
	#superclass : 'OCValueNode',
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
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCBlockNode class >> arguments: argNodes body: sequenceNode [
	^(self new)
		arguments: argNodes;
		body: sequenceNode;
		yourself
]

{ #category : 'instance creation' }
OCBlockNode class >> body: sequenceNode [
	^self arguments: #() body: sequenceNode
]

{ #category : 'comparing' }
OCBlockNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self body = anObject body ifFalse: [^false].
	self arguments size = anObject arguments size ifFalse: [^false].
	self arguments with: anObject arguments do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : 'visiting' }
OCBlockNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitBlockNode: self
]

{ #category : 'accessing' }
OCBlockNode >> allArgumentVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allArgumentVariables;
		yourself
]

{ #category : 'accessing' }
OCBlockNode >> allDefinedVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself
]

{ #category : 'accessing' }
OCBlockNode >> allStatements [
	"including temp variable definition."

	^ (OrderedCollection withAll: self temporaries)
		  addAll: super allStatements;
		  yourself
]

{ #category : 'accessing' }
OCBlockNode >> argumentNames [
	^ self arguments collect: [:each | each name] as: Array
]

{ #category : 'accessing' }
OCBlockNode >> arguments [
	^arguments
]

{ #category : 'accessing' }
OCBlockNode >> arguments: argCollection [
	arguments := argCollection.
	arguments do: [:each | each parent: self ]
]

{ #category : 'accessing - token' }
OCBlockNode >> bar [
	^ bar
]

{ #category : 'accessing - token' }
OCBlockNode >> bar: anInteger [
	bar := anInteger
]

{ #category : 'accessing' }
OCBlockNode >> blockVariables [
	| vars |
	vars := super blockVariables asOrderedCollection.
	vars addAll: self argumentNames.
	^vars
]

{ #category : 'accessing' }
OCBlockNode >> body [
	^body
]

{ #category : 'accessing' }
OCBlockNode >> body: stmtsNode [
	body := stmtsNode.
	body parent: self
]

{ #category : 'accessing' }
OCBlockNode >> children [
	^ arguments copyWith: body
]

{ #category : 'accessing - token' }
OCBlockNode >> colons [
	^ colons
]

{ #category : 'accessing - token' }
OCBlockNode >> colons: anArray [
	colons := anArray
]

{ #category : 'accessing' }
OCBlockNode >> constantValue [
	self isConstant ifFalse: [ self error: 'block is not constant' ].
	^ body statements
		ifEmpty: [ nil ]
		ifNotEmpty: [:statements | statements first value ]
]

{ #category : 'matching' }
OCBlockNode >> copyInContext: aDictionary [
	^ self class new
		arguments: (self copyList: self arguments inContext: aDictionary);
		body: (self body copyInContext: aDictionary);
		yourself
]

{ #category : 'testing' }
OCBlockNode >> defines: aName [
	^arguments anySatisfy: [:each | each name = aName]
]

{ #category : 'comparing' }
OCBlockNode >> equalTo: anObject withMapping: aDictionary [
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

{ #category : 'testing' }
OCBlockNode >> hasArgumentNamed: aString [
	^ self arguments anySatisfy: [ :argument| argument name = aString ]
]

{ #category : 'testing' }
OCBlockNode >> hasArguments [

	^ arguments isNotEmpty
]

{ #category : 'testing' }
OCBlockNode >> hasBlock [

	^ true
]

{ #category : 'testing' }
OCBlockNode >> hasBlockReturn [

	self deprecated: 'This method is deprecated, don''t use it. Use on your own responsibility.'.

	^ self body lastIsReturn
]

{ #category : 'testing' }
OCBlockNode >> hasSameExitPoint [

	^ body hasSameExitPoint 
]

{ #category : 'testing' }
OCBlockNode >> hasSameExitPoint: aBoolean [ 

	^ body hasSameExitPoint: aBoolean 
]

{ #category : 'testing' }
OCBlockNode >> hasTemporaries [

	^ self temporaries isNotEmpty
]

{ #category : 'testing' }
OCBlockNode >> hasTemporaryNamed: aString [
	^ self temporaries anySatisfy: [ :temp| temp name = aString ]
]

{ #category : 'comparing' }
OCBlockNode >> hash [
	^ (self hashForCollection: self arguments) bitXor: self body hash
]

{ #category : 'testing' }
OCBlockNode >> headIsNotEmpty [
	"Return true whether the receiver has some arguments or comments"

	^ arguments isNotEmpty or: [ self comments isNotEmpty ]
]

{ #category : 'initialization' }
OCBlockNode >> initialize [
	super initialize.

	arguments := #().
	colons := #().
	left := 0.
	right := 0
]

{ #category : 'testing' }
OCBlockNode >> isBlock [

	^ true
]

{ #category : 'testing' }
OCBlockNode >> isConstant [
	"is the block just returning a literal?"
	^ body statements
		ifEmpty: [ true "empty block returns nil" ]
		ifNotEmpty: [:statements | statements size = 1 and: [statements first isLiteralNode] ]
]

{ #category : 'errors' }
OCBlockNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^(self arguments anySatisfy: [:each | each isFaulty] ) or: [ self body isFaulty]
]

{ #category : 'testing' }
OCBlockNode >> isImmediateNode [
	^true
]

{ #category : 'testing' }
OCBlockNode >> isLast: aNode [
	^body isLast: aNode
]

{ #category : 'testing' }
OCBlockNode >> isUsingAsReturnValue: aNode [
	aNode = body ifFalse: [^false].
	^parent isMessage
		ifTrue:
			[(#(#ifTrue:ifFalse: #ifTrue: #ifFalse: #ifFalse:ifTrue:)
				includes: parent selector) not
				or: [parent isUsedAsReturnValue]]
		ifFalse: [self isUsedAsReturnValue]
]

{ #category : 'accessing - token' }
OCBlockNode >> left [
	^ left
]

{ #category : 'accessing - token' }
OCBlockNode >> left: anInteger [
	left := anInteger
]

{ #category : 'matching' }
OCBlockNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^(self
		matchList: arguments
		against: aNode arguments
		inContext: aDictionary)
			and: [body match: aNode body inContext: aDictionary]
]

{ #category : 'accessing' }
OCBlockNode >> methodOrBlockNode [
	^ self
]

{ #category : 'testing' }
OCBlockNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
OCBlockNode >> numArgs [
	^ self arguments size
]

{ #category : 'copying' }
OCBlockNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ]).
	self body: self body copy
]

{ #category : 'accessing' }
OCBlockNode >> precedence [
	^0
]

{ #category : 'testing' }
OCBlockNode >> references: aVariableName [
	^body references: aVariableName
]

{ #category : 'adding-removing' }
OCBlockNode >> removeTemporaryNamed: aName [

	self body removeTemporaryNamed: aName
]

{ #category : 'replacing' }
OCBlockNode >> replaceNode: aNode withNode: anotherNode [
	body == aNode ifTrue: [self body: anotherNode].
	self arguments: (arguments
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'accessing - token' }
OCBlockNode >> right [
	^ right
]

{ #category : 'accessing - token' }
OCBlockNode >> right: anInteger [
	right := anInteger
]

{ #category : 'accessing' }
OCBlockNode >> scope [
	^ scope
]

{ #category : 'accessing' }
OCBlockNode >> scope: aScopedNode [
	scope := aScopedNode
]

{ #category : 'accessing' }
OCBlockNode >> startWithoutParentheses [
	^ left
]

{ #category : 'accessing' }
OCBlockNode >> statementComments [
	"this is used for formatting comments for this node as a statement, but I don't have a single statement comment, my comments are place within the block code"

	^ ''
]

{ #category : 'accessing' }
OCBlockNode >> statements [
	^ self body statements
]

{ #category : 'accessing' }
OCBlockNode >> stopWithoutParentheses [
	^ right
]

{ #category : 'accessing' }
OCBlockNode >> temporaries [
	^ self body temporaries
]

{ #category : 'accessing' }
OCBlockNode >> temporaryNames [
	^ self body temporaryNames
]
