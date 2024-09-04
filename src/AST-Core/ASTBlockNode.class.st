"
RBBlockNode is an AST node that represents a block ""[...]"".

Like RBMethodNode, the scope attribute is only valid after doing a semantic analyzing step.

Instance Variables:
	arguments	<SequenceableCollection of: ASTVariableNode>	the arguments for the block
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
	#name : 'ASTBlockNode',
	#superclass : 'ASTValueNode',
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
ASTBlockNode class >> arguments: argNodes body: sequenceNode [
	^(self new)
		arguments: argNodes;
		body: sequenceNode;
		yourself
]

{ #category : 'instance creation' }
ASTBlockNode class >> body: sequenceNode [
	^self arguments: #() body: sequenceNode
]

{ #category : 'comparing' }
ASTBlockNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self body = anObject body ifFalse: [^false].
	self arguments size = anObject arguments size ifFalse: [^false].
	self arguments with: anObject arguments do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : 'visiting' }
ASTBlockNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitBlockNode: self
]

{ #category : 'accessing' }
ASTBlockNode >> allArgumentVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allArgumentVariables;
		yourself
]

{ #category : 'accessing' }
ASTBlockNode >> allDefinedVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself
]

{ #category : 'accessing' }
ASTBlockNode >> allStatements [
	"including temp variable definition."

	^ (OrderedCollection withAll: self temporaries)
		  addAll: super allStatements;
		  yourself
]

{ #category : 'accessing' }
ASTBlockNode >> argumentNames [
	^ self arguments collect: [:each | each name] as: Array
]

{ #category : 'accessing' }
ASTBlockNode >> arguments [
	^arguments
]

{ #category : 'accessing' }
ASTBlockNode >> arguments: argCollection [
	arguments := argCollection.
	arguments do: [:each | each parent: self ]
]

{ #category : 'accessing - token' }
ASTBlockNode >> bar [
	^ bar
]

{ #category : 'accessing - token' }
ASTBlockNode >> bar: anInteger [
	bar := anInteger
]

{ #category : 'accessing' }
ASTBlockNode >> blockVariables [
	| vars |
	vars := super blockVariables asOrderedCollection.
	vars addAll: self argumentNames.
	^vars
]

{ #category : 'accessing' }
ASTBlockNode >> body [
	^body
]

{ #category : 'accessing' }
ASTBlockNode >> body: stmtsNode [
	body := stmtsNode.
	body parent: self
]

{ #category : 'accessing' }
ASTBlockNode >> children [
	^ arguments copyWith: body
]

{ #category : 'accessing - token' }
ASTBlockNode >> colons [
	^ colons
]

{ #category : 'accessing - token' }
ASTBlockNode >> colons: anArray [
	colons := anArray
]

{ #category : 'accessing' }
ASTBlockNode >> constantValue [
	self isConstant ifFalse: [ self error: 'block is not constant' ].
	^ body statements
		ifEmpty: [ nil ]
		ifNotEmpty: [:statements | statements first value ]
]

{ #category : 'matching' }
ASTBlockNode >> copyInContext: aDictionary [
	^ self class new
		arguments: (self copyList: self arguments inContext: aDictionary);
		body: (self body copyInContext: aDictionary);
		yourself
]

{ #category : 'testing' }
ASTBlockNode >> defines: aName [
	^arguments anySatisfy: [:each | each name = aName]
]

{ #category : 'comparing' }
ASTBlockNode >> equalTo: anObject withMapping: aDictionary [
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
ASTBlockNode >> hasArgumentNamed: aString [
	^ self arguments anySatisfy: [ :argument| argument name = aString ]
]

{ #category : 'testing' }
ASTBlockNode >> hasArguments [

	^ arguments isNotEmpty
]

{ #category : 'testing' }
ASTBlockNode >> hasBlock [

	^ true
]

{ #category : 'testing' }
ASTBlockNode >> hasBlockReturn [

	self deprecated: 'This method is deprecated, don''t use it. Use on your own responsibility.'.

	^ self body lastIsReturn
]

{ #category : 'testing' }
ASTBlockNode >> hasSameExitPoint [

	^ body hasSameExitPoint 
]

{ #category : 'testing' }
ASTBlockNode >> hasSameExitPoint: aBoolean [ 

	^ body hasSameExitPoint: aBoolean 
]

{ #category : 'testing' }
ASTBlockNode >> hasTemporaries [

	^ self temporaries isNotEmpty
]

{ #category : 'testing' }
ASTBlockNode >> hasTemporaryNamed: aString [
	^ self temporaries anySatisfy: [ :temp| temp name = aString ]
]

{ #category : 'comparing' }
ASTBlockNode >> hash [
	^ (self hashForCollection: self arguments) bitXor: self body hash
]

{ #category : 'testing' }
ASTBlockNode >> headIsNotEmpty [
	"Return true whether the receiver has some arguments or comments"

	^ arguments isNotEmpty or: [ self comments isNotEmpty ]
]

{ #category : 'initialization' }
ASTBlockNode >> initialize [
	super initialize.

	arguments := #().
	colons := #().
	left := 0.
	right := 0
]

{ #category : 'testing' }
ASTBlockNode >> isBlock [

	^ true
]

{ #category : 'testing' }
ASTBlockNode >> isConstant [
	"is the block just returning a literal?"
	^ body statements
		ifEmpty: [ true "empty block returns nil" ]
		ifNotEmpty: [:statements | statements first isLiteralNode ]
]

{ #category : 'errors' }
ASTBlockNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^(self arguments anySatisfy: [:each | each isFaulty] ) or: [ self body isFaulty]
]

{ #category : 'testing' }
ASTBlockNode >> isImmediateNode [
	^true
]

{ #category : 'testing' }
ASTBlockNode >> isLast: aNode [
	^body isLast: aNode
]

{ #category : 'testing' }
ASTBlockNode >> isUsingAsReturnValue: aNode [
	aNode = body ifFalse: [^false].
	^parent isMessage
		ifTrue:
			[(#(#ifTrue:ifFalse: #ifTrue: #ifFalse: #ifFalse:ifTrue:)
				includes: parent selector) not
				or: [parent isUsedAsReturnValue]]
		ifFalse: [self isUsedAsReturnValue]
]

{ #category : 'accessing - token' }
ASTBlockNode >> left [
	^ left
]

{ #category : 'accessing - token' }
ASTBlockNode >> left: anInteger [
	left := anInteger
]

{ #category : 'matching' }
ASTBlockNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^(self
		matchList: arguments
		against: aNode arguments
		inContext: aDictionary)
			and: [body match: aNode body inContext: aDictionary]
]

{ #category : 'accessing' }
ASTBlockNode >> methodOrBlockNode [
	^ self
]

{ #category : 'testing' }
ASTBlockNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
ASTBlockNode >> numArgs [
	^ self arguments size
]

{ #category : 'copying' }
ASTBlockNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ]).
	self body: self body copy
]

{ #category : 'accessing' }
ASTBlockNode >> precedence [
	^0
]

{ #category : 'testing' }
ASTBlockNode >> references: aVariableName [
	^body references: aVariableName
]

{ #category : 'adding-removing' }
ASTBlockNode >> removeTemporaryNamed: aName [

	self body removeTemporaryNamed: aName
]

{ #category : 'replacing' }
ASTBlockNode >> replaceNode: aNode withNode: anotherNode [
	body == aNode ifTrue: [self body: anotherNode].
	self arguments: (arguments
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'accessing - token' }
ASTBlockNode >> right [
	^ right
]

{ #category : 'accessing - token' }
ASTBlockNode >> right: anInteger [
	right := anInteger
]

{ #category : 'accessing' }
ASTBlockNode >> scope [
	^ scope
]

{ #category : 'accessing' }
ASTBlockNode >> scope: aScopedNode [
	scope := aScopedNode
]

{ #category : 'accessing' }
ASTBlockNode >> startWithoutParentheses [
	^ left
]

{ #category : 'accessing' }
ASTBlockNode >> statementComments [
	"this is used for formatting comments for this node as a statement, but I don't have a single statement comment, my comments are place within the block code"

	^ ''
]

{ #category : 'accessing' }
ASTBlockNode >> statements [
	^ self body statements
]

{ #category : 'accessing' }
ASTBlockNode >> stopWithoutParentheses [
	^ right
]

{ #category : 'accessing' }
ASTBlockNode >> temporaries [
	^ self body temporaries
]

{ #category : 'accessing' }
ASTBlockNode >> temporaryNames [
	^ self body temporaryNames
]
