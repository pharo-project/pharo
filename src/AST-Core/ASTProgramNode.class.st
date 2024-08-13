"
ASTProgramNode is an abstract class that represents an abstract syntax tree node in a Smalltalk program.

Subclasses must implement the following messages:
	accessing
		start
		stop
	visitor
		acceptVisitor:
	testing
		isFaulty

The #start and #stop methods are used to find the source that corresponds to this node. ""source copyFrom: self start to: self stop"" should return the source for this node.

The #acceptVisitor: method is used by RBProgramNodeVisitors (the visitor pattern). This will also require updating all the RBProgramNodeVisitors so that they know of the new node.

The #isFaulty method is used to distinguish between valid nodes and nodes created from an invalid source Smalltalk code. For example, code parsed with RBParsers #parseFaultyExpression: or #parseFaultyMethod:.

Subclasses might also want to redefine match:inContext: and copyInContext: to do parse tree searching and replacing.

Subclasses that contain other nodes should override equalTo:withMapping: to compare nodes while ignoring renaming temporary variables, and children that returns a collection of our children nodes.

Instance Variables:
	properties	<Dictionary of: Symbol -> Object>	A set of properties set to this node, for example every node can have the Property #comment to attach the method comment or the comment of the code line this node represents. Other classes or tools may add more type of properties; for example, the reflectivity support adds properties for managing Metalinks. 
	parent	<ASTProgramNode>	the node we're contained in

Class Variables:
	FormatterClass	<Behavior>	the formatter class that is used when we are formatted

"
Class {
	#name : 'ASTProgramNode',
	#superclass : 'ASTNode',
	#instVars : [
		'parent',
		'properties'
	],
	#classVars : [
		'FormatterClass'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'accessing' }
ASTProgramNode class >> formatterClass [
	^ FormatterClass ifNil: [ RBSimpleFormatter ] ifNotNil: [ FormatterClass ]
]

{ #category : 'accessing' }
ASTProgramNode class >> formatterClass: aClass [
	FormatterClass := aClass
]

{ #category : 'testing' }
ASTProgramNode class >> isAbstract [

	^ self == ASTProgramNode
]

{ #category : 'accessing' }
ASTProgramNode class >> optimizedSelectors [
	^ #( and: caseOf: caseOf:otherwise: ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: ifTrue: ifTrue:ifFalse: or: to:by:do: to:do: whileFalse whileFalse: whileTrue whileTrue: )
]

{ #category : 'accessing' }
ASTProgramNode class >> resetFormatter [

	self formatterClass: nil
]

{ #category : 'notice' }
ASTProgramNode >> addError: anErrorMessage [
	"Add an error information to the node. Node with error information are considered faulty.
	Semantic passes that check the validity of AST are the targetted clients of this method."

	| notice |
	notice := RBErrorNotice new messageText: anErrorMessage.
	self addNotice: notice.
	^ notice
]

{ #category : 'notice' }
ASTProgramNode >> addNotice: aRBNotice [

	(self propertyAt: #notices ifAbsentPut: [ OrderedCollection new ]) add: aRBNotice.
	aRBNotice node: self
]

{ #category : 'replacing' }
ASTProgramNode >> addReplacement: aStringReplacement [
	parent ifNil: [^self].
	parent addReplacement: aStringReplacement
]

{ #category : 'notice' }
ASTProgramNode >> addWarning: aMessage [

	| notice |
	"Add a warning information to the node.
	Semantic passes that check the validity of AST are the targetted clients of this method."
	notice := RBWarningNotice new messageText: aMessage.
	self addNotice: notice.
	^ notice
]

{ #category : 'accessing' }
ASTProgramNode >> allArgumentVariables [
	| children |
	children := self children.
	children isEmpty ifTrue: [^#()].
	^children inject: OrderedCollection new
		into:
			[:vars :each |
			vars
				addAll: each allArgumentVariables;
				yourself]
]

{ #category : 'iterating' }
ASTProgramNode >> allChildren [
	| children |
	children := OrderedCollection new.
	self nodesDo: [ :each | children addLast: each ].
	^ children
]

{ #category : 'accessing' }
ASTProgramNode >> allComments [
	"Answer a collection of objects representing the comments in the method. Return an empty collection if the method's source code does not contain a comment."

	^ self allChildren flatCollect: [:el| el comments]
]

{ #category : 'accessing' }
ASTProgramNode >> allDefinedVariables [
	| children |
	children := self children.
	children isEmpty ifTrue: [^#()].
	^children inject: OrderedCollection new
		into:
			[:vars :each |
			vars
				addAll: each allDefinedVariables;
				yourself]
]

{ #category : 'notice' }
ASTProgramNode >> allErrorNotices [
	"Include also error notices of all children"

	| result |
	result := OrderedCollection new.
	self nodesPostorderDo: [ :node | result addAll: node errorNotices ].
	^ result
]

{ #category : 'notice' }
ASTProgramNode >> allNotices [
	"Include also notices of all children"

	| result |
	result := OrderedCollection new.
	self nodesPostorderDo: [ :node | result addAll: node notices ].
	^ result
]

{ #category : 'accessing' }
ASTProgramNode >> allParents [
	"return all my parents.
	See discussion in issue https://github.com/pharo-project/pharo/issues/6278"
	^ parent
		ifNil: [ OrderedCollection new ]
		ifNotNil: [ parent withAllParents ]
]

{ #category : 'accessing' }
ASTProgramNode >> allStatements [
	| children |
	children := self children.
	children isEmpty ifTrue: [^#()].
	^children inject: OrderedCollection new
		into:
			[:vars :each |
			vars
				addAll: each allStatements;
				yourself]
]

{ #category : 'accessing' }
ASTProgramNode >> allTemporaryVariables [
	| children |
	children := self children.
	children isEmpty ifTrue: [^#()].
	^children inject: OrderedCollection new
		into:
			[:vars :each |
			vars
				addAll: each allTemporaryVariables;
				yourself]
]

{ #category : 'accessing' }
ASTProgramNode >> allVariables [
	"Return all the variables in subnodes"

	^ self allChildren select: [ :each | each isVariable ]
]

{ #category : 'replacing' }
ASTProgramNode >> asDoit [
	"The VM can only evaluate methods. wrap this ast in a doit MethodNode"

	| source |
	source := self source.
	^ RBDoItMethodNode new
		  body: self asSequenceNode;
		  source: source
]

{ #category : 'tests' }
ASTProgramNode >> asPositionDebugString [
	"Compute a string representing the positions of children nodes.
	Each character is an identifier (0 to 9 then A to Z) that corresponds to the chidren node at the corresponding position.
	Identifiers are assigned with a deep-first search: self is 0, first chisdren is 1, etc."

	| cpt characters result |
	"DFS to assign identifiers to nodes"
	cpt := 0.
	characters := IdentityDictionary new.
	characters at: nil put: Character space.
	self nodesDo: [ :each |
		characters at: each put: cpt asCharacterDigit.
		cpt := cpt + 1.
		each comments do: [ :eachCmt |
			characters at: eachCmt put: cpt asCharacterDigit.
			cpt := cpt + 1 ] ].

	"Fill the result string"
	result := String new: self source size.
	1 to: self source size do: [ :i |
		result
			at: i
			put: (characters at: (self nodeForOffset: i)) ].
	^ result
]

{ #category : 'replacing' }
ASTProgramNode >> asReturn [
	"Change the current node to a return node."

	parent ifNil: [self error: 'Cannot change to a return without a parent node.'].
	parent isSequence
		ifFalse: [self error: 'Parent node must be a sequence node.'].
	(parent isLast: self) ifFalse: [self error: 'Return node must be last.'].
	^parent addReturn
]

{ #category : 'replacing' }
ASTProgramNode >> asSequenceNode [
	^RBSequenceNode statements: {self}
]

{ #category : 'querying' }
ASTProgramNode >> assignmentNodes [
	^self allChildren select: [:each | each isAssignment]
]

{ #category : 'testing' }
ASTProgramNode >> assigns: aVariableName [
	^self children anySatisfy: [:each | each assigns: aVariableName]
]

{ #category : 'querying' }
ASTProgramNode >> bestNodeFor: anInterval [
	| selectedChildren selectedComments |
	anInterval ifNil: [^nil ].
	selectedComments := self getCommentsFor: anInterval.
	(self intersectsInterval: anInterval) ifFalse: [^selectedComments ifNil: [ nil ] ifNotNil: [ selectedComments size = 1 ifTrue: [ selectedComments first ] ifFalse: [ nil ] ] ].
	(self containedBy: anInterval) ifTrue: [^self].

	selectedChildren := self children select: [:each |
		each intersectsInterval: anInterval
	].

	^ selectedChildren size = 1
		ifTrue: [ selectedChildren first bestNodeFor: anInterval ]
		ifFalse: [ (selectedChildren isEmpty and: [ selectedComments size = 1 ])
							ifTrue: [ selectedComments first ]
							ifFalse: [ self ] ]
]

{ #category : 'querying' }
ASTProgramNode >> bestNodeForPosition: aPosition [

	"aPosition is an integer that represents the position of the caret.
	A value of N represents that the caret is between the characters N-1 and N.
	The position 1 is at beginning of the text.

	Position Heuristic: If the previous character is not a separator, take selection one position before.
		This heuristic is for the cases where the caret (|) is:
		   |self foo  => the caret is before self, do not move
		   self foo|  => the caret is before foo, interpret is as if we are in foo.
			self foo | => the caret is before a space, interpret is as if we are in foo.

		This heuristic introduces although an ambiguity when code is not nicely formatted:
		   self foo:|#bar => Here a user may want foo: or bar.
		For now we decided to favor foo: to motivate people to indent code correctly	"

	| offset position precededBySeparator |
	precededBySeparator := [
	                       (aPosition
		                        between: 2
		                        and: self methodNode sourceCode size + 1)
		                       and: [
		                       (self methodNode sourceCode at: aPosition - 1)
			                       isSeparator ] ].
	offset := (aPosition = 1 or: precededBySeparator)
		          ifTrue: [ 0 ]
		          ifFalse: [ -1 ].

	position := aPosition + offset min: self stop.

	^ self bestNodeFor: (position to: position)
]

{ #category : 'querying' }
ASTProgramNode >> blockNodes [
	^self allChildren select: [:each | each isBlock]
]

{ #category : 'accessing' }
ASTProgramNode >> blockVariables [
	^parent ifNil: [#()] ifNotNil: [parent blockVariables]
]

{ #category : 'testing - matching' }
ASTProgramNode >> canMatchMethod: aCompiledMethod [
	^self sentMessages allSatisfy:
			[:each |
			(self class optimizedSelectors includes: each)
				or: [aCompiledMethod refersToLiteral: each]]
]

{ #category : 'accessing - meta variable' }
ASTProgramNode >> cascadeListCharacter [
	^$;
]

{ #category : 'notice' }
ASTProgramNode >> checkFaulty: aBlock [
	"Helper method to evaluate errorBlock and/or signal SyntaxErrorNotification in case of a faulty AST"

	| errorNotice |
	"Fast noop case"
	self isFaulty ifFalse: [ ^ self ].

	"Search for the first error, to signal"
	errorNotice := self allErrorNotices first.

	aBlock ifNotNil: [ aBlock cull: errorNotice messageText cull: errorNotice position cull: self ].

	errorNotice signalError.

	"If resumed, just return"
]

{ #category : 'accessing' }
ASTProgramNode >> children [
	^#()
]

{ #category : 'accessing' }
ASTProgramNode >> childrenAndComments [
	
	| result |
	result := self children.
	result ifEmpty: [ ^ self comments ].
	self comments ifEmpty: [ ^ result ].
	^ result , self comments
]

{ #category : 'replacing' }
ASTProgramNode >> clearReplacements [
	parent ifNil: [^self].
	parent clearReplacements
]

{ #category : 'enumeration' }
ASTProgramNode >> collect: aBlock [
	"Hacked to fit collection protocols"

	^aBlock value: self
]

{ #category : 'accessing' }
ASTProgramNode >> comments [
	"Answer the comments of the receiving parse tree node"

	^ (self propertyAt: #comments ifAbsent: [ #() ]) ifNil:[ #() ]
]

{ #category : 'accessing' }
ASTProgramNode >> comments: aCollection [

	(aCollection ifNil: [#()])
		ifEmpty: [ self removeProperty: #comments ifAbsent: [] ]
		ifNotEmpty: [ self propertyAt: #comments put: aCollection ]
]

{ #category : 'testing' }
ASTProgramNode >> containedBy: anInterval [
	^anInterval first <= self start and: [anInterval last >= self stop]
]

{ #category : 'testing' }
ASTProgramNode >> containsReturn [
	^self children anySatisfy: [:each | each containsReturn]
]

{ #category : 'copying' }
ASTProgramNode >> copyCommentsFrom: aNode [
	"Add all comments from aNode to us. If we already have the comment, then don't add it."

	| newComments |
	newComments := OrderedCollection new.
	aNode nodesDo: [:each | newComments addAll: each comments].
	self nodesDo:
			[:each |
			each comments do: [:comment | newComments remove: comment ifAbsent: []]].
	newComments isEmpty ifTrue: [^self].
	newComments := newComments asSortedCollection: [:a :b | a start < b start].
	self comments: newComments
]

{ #category : 'matching' }
ASTProgramNode >> copyInContext: aDictionary [
	^ self copy
]

{ #category : 'matching' }
ASTProgramNode >> copyList: matchNodes inContext: aDictionary [
	| newNodes |
	newNodes := OrderedCollection new.
	matchNodes do:
			[:each |
			| object |
			object := each copyInContext: aDictionary.
			newNodes addAll: object].
	^newNodes
]

{ #category : 'accessing' }
ASTProgramNode >> debugHighlightRange [

	^ self debugHighlightStart to: self debugHighlightStop
]

{ #category : 'accessing' }
ASTProgramNode >> debugHighlightStart [

	^ self start
]

{ #category : 'accessing' }
ASTProgramNode >> debugHighlightStop [

	^ self stop
]

{ #category : 'testing' }
ASTProgramNode >> defines: aName [
	^false
]

{ #category : 'enumeration' }
ASTProgramNode >> do: aBlock [
	"Hacked to fit collection protocols"

	aBlock value: self
]

{ #category : 'generation' }
ASTProgramNode >> dump [
	"Generate a literal expression that recreates the receiver"

	| visitor |
	visitor := RBDumpVisitor new.
	self acceptVisitor: visitor.
	^ visitor contents
]

{ #category : 'comparing' }
ASTProgramNode >> equalTo: aNode exceptForVariables: variableNameCollection [
	| dictionary |
	dictionary := Dictionary new.
	(self equalTo: aNode withMapping: dictionary) ifFalse: [^false].
	dictionary keysAndValuesDo:
			[:key :value |
			(key = value or: [variableNameCollection includes: key]) ifFalse: [^false]].
	^true
]

{ #category : 'comparing' }
ASTProgramNode >> equalTo: aNode withMapping: aDictionary [
	^self = aNode
]

{ #category : 'notice' }
ASTProgramNode >> errorNotices [
	"The errors attached to the receiver"

	| notices |
	
	notices := self notices.
	
	"Optimization to avoid select in an empty collection"
	notices ifEmpty: [ ^ #() ].

	^ notices select: [ :e | e isError ]
]

{ #category : 'testing' }
ASTProgramNode >> evaluatedFirst: aNode [
	self children do:
			[:each |
			each == aNode ifTrue: [^true].
			each isImmediateNode ifFalse: [^false]].
	^false
]

{ #category : 'printing' }
ASTProgramNode >> formattedCode [

	^ self formattedCodeIn: self class
]

{ #category : 'printing' }
ASTProgramNode >> formattedCodeIn: context [

	^ context formatter format: self
]

{ #category : 'accessing' }
ASTProgramNode >> formatterClass [
	^ self class formatterClass
]

{ #category : 'querying' }
ASTProgramNode >> getCommentsFor: anInterval [
	| selectedComments |
	selectedComments := OrderedCollection new.
	self nodesDo:
		[ :each |
			| comments |
			comments := each comments select:
				[ :com | com intersectsInterval: anInterval ].
	"This precaution is taken to deal with cascades multiple visit of the receiver."
			(selectedComments isNotEmpty
				and: [ comments isNotEmpty
				and: [ comments last = selectedComments last ]])
					ifFalse: [selectedComments addAll: comments ] ].
	^ selectedComments
]

{ #category : 'testing' }
ASTProgramNode >> hasBlock [

	^ false
]

{ #category : 'testing' }
ASTProgramNode >> hasComments [
	"Answer whether the receiver as comments"

	^ self comments isNotEmpty
]

{ #category : 'testing' }
ASTProgramNode >> hasMultipleReturns [
	| count |
	count := 0.
	self nodesDo: [:each | each isReturn ifTrue: [count := count + 1]].
	^count > 1
]

{ #category : 'testing' }
ASTProgramNode >> hasNonLocalReturn [
	"check if there is a non-local return anywhere
	Note: returns in a method itself are local returns"
	^ self children anySatisfy: [ :child | child hasNonLocalReturn ]
]

{ #category : 'properties' }
ASTProgramNode >> hasProperty: aKey [
	"Test if the property aKey is present."

	^ properties isNotNil and: [ properties includesKey: aKey ]
]

{ #category : 'testing' }
ASTProgramNode >> hasSameExitPoint [

	^ self hasSameExitPoint: false
]

{ #category : 'testing' }
ASTProgramNode >> hasSameExitPoint: aBoolean [ 

	^ true
]

{ #category : 'comparing' }
ASTProgramNode >> hashForCollection: aCollection [
	^ aCollection isEmpty ifTrue: [ 0 ] ifFalse: [ aCollection first hash ]
]

{ #category : 'querying' }
ASTProgramNode >> instanceVariableNodes [
		^self variableNodes select: [:each | each isInstanceVariable]
]

{ #category : 'querying' }
ASTProgramNode >> instanceVariableReadNodes [
		^self variableReadNodes select: [:each | each isInstanceVariable]
]

{ #category : 'querying' }
ASTProgramNode >> instanceVariableWriteNodes [
		^self variableWriteNodes select: [:each | each isInstanceVariable]
]

{ #category : 'testing' }
ASTProgramNode >> intersectsInterval: anInterval [
	^(anInterval first between: self start and: self stop)
		or: [self start between: anInterval first and: anInterval last]
]

{ #category : 'testing' }
ASTProgramNode >> isAnnotationMark [
	^ false
]

{ #category : 'testing' }
ASTProgramNode >> isArgumentVariable [
	^ false
]

{ #category : 'testing' }
ASTProgramNode >> isArrayError [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isAssignment [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isBlockError [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isCascade [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isCascadeError [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isClassVariable [
	^ false
]

{ #category : 'testing' }
ASTProgramNode >> isCommentNode [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isDynamicArray [
	^ false
]

{ #category : 'testing' }
ASTProgramNode >> isEnglobingError [
	^false
]

{ #category : 'notice' }
ASTProgramNode >> isError [
	"An error node is either a RBParseErrorNode (for syntactic errors) or has error information (for other errors)"

	^ self errorNotices isNotEmpty
]

{ #category : 'testing' }
ASTProgramNode >> isEssential [
	"This node can not be removed from the expression, else the expression is not valid anymore.
	 An argument, receiver, or part of an assignment or part of a return can not be removed.
	 This ensures that if the node would be removed, a resulting expression will still be syntatically correct.
     For example, self foo can not be removed from the following:
	^ self foo
	x := self foo
	self foo bar
	self arg: self foo"

	^parent ifNil: [false] ifNotNil: [parent isEssentialChild: self]
]

{ #category : 'testing' }
ASTProgramNode >> isEssentialChild: aNode [
	^true
]

{ #category : 'testing' }
ASTProgramNode >> isEvaluatedFirst [
	"Return true if we are the first thing evaluated in this statement."

	^parent isNil or: [parent isSequence or: [parent evaluatedFirst: self]]
]

{ #category : 'notice' }
ASTProgramNode >> isFaulty [

	"return true if the AST is or contains a isError node."
	^ self isError
]

{ #category : 'testing' }
ASTProgramNode >> isGlobalVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isHaltNode [
	^ false
]

{ #category : 'testing' }
ASTProgramNode >> isImmediateNode [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isInstanceVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isLast: aNode [
	| children |
	children := self children.
	^children notEmpty and: [children last == aNode]
]

{ #category : 'testing - matching' }
ASTProgramNode >> isList [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isLiteralArray [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isLiteralArrayError [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isLiteralNode [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isLiteralVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isLocalVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isMessage [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isMethod [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isParenthesesError [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isParseError [
	^false
]

{ #category : 'testing - matching' }
ASTProgramNode >> isPatternNode [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isPragma [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isPragmaError [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isPseudoVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isReservedVariable [
	self
		deprecated: 'Please use #isPseudoVariable instead'
		transformWith: '`@receiver isReservedVariable' -> '`@receiver isPseudoVariable'.

	^ self isPseudoVariable
]

{ #category : 'testing' }
ASTProgramNode >> isReturn [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isSelector [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isSelfOrSuperVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isSelfVariable [
	^ false
]

{ #category : 'testing' }
ASTProgramNode >> isSequence [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isSuperVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isTempVariable [
	^ false
]

{ #category : 'testing' }
ASTProgramNode >> isTemporariesError [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isThisContextVariable [
	^ false
]

{ #category : 'testing' }
ASTProgramNode >> isUndeclaredVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isUnfinishedStatement [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isUsedAsReturnValue [
	"Answer true if this node could be used as part of another expression. For example, you could use the
	result of this node as a receiver of a message, an argument, the right part of an assignment, or the
	return value of a block. This differs from isDirectlyUsed in that it is conservative since it also includes
	return values of blocks."

	^parent ifNil: [false] ifNotNil: [parent isUsingAsReturnValue: self]
]

{ #category : 'testing' }
ASTProgramNode >> isUsingAsReturnValue: aNode [
	^true
]

{ #category : 'testing' }
ASTProgramNode >> isValue [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> isWorkspaceVariable [
	^false
]

{ #category : 'testing' }
ASTProgramNode >> lastIsReturn [
	^ false
]

{ #category : 'accessing - meta variable' }
ASTProgramNode >> listCharacter [
	^$@
]

{ #category : 'accessing - meta variable' }
ASTProgramNode >> literalCharacter [
	^$#
]

{ #category : 'accessing' }
ASTProgramNode >> mappingFor: aNode [
	| method |
	method := self methodNode.
	method ifNil: [^aNode].
	^method mappingFor: aNode
]

{ #category : 'matching' }
ASTProgramNode >> match: aNode inContext: aDictionary [
	^ self = aNode
]

{ #category : 'matching' }
ASTProgramNode >> matchList: matchNodes against: programNodes inContext: aDictionary [
	^self
		matchList: matchNodes
		index: 1
		against: programNodes
		index: 1
		inContext: aDictionary
]

{ #category : 'matching' }
ASTProgramNode >> matchList: matchNodes index: matchIndex against: programNodes index: programIndex inContext: aDictionary [
	| node currentIndex currentDictionary nodes |
	matchNodes size < matchIndex ifTrue: [^programNodes size < programIndex].
	node := matchNodes at: matchIndex.
	node isList
		ifTrue:
			[currentIndex := programIndex - 1.

			[currentDictionary := aDictionary copy.
			programNodes size < currentIndex or:
					[nodes := programNodes copyFrom: programIndex to: currentIndex.
					(currentDictionary at: node ifAbsentPut: [nodes]) = nodes and:
							[(self
								matchList: matchNodes
								index: matchIndex + 1
								against: programNodes
								index: currentIndex + 1
								inContext: currentDictionary)
									ifTrue:
										[currentDictionary
											keysAndValuesDo: [:key :value | aDictionary at: key put: value].
										^true].
							false]]]
					whileFalse: [currentIndex := currentIndex + 1].
			^false].
	programNodes size < programIndex ifTrue: [^false].
	(node match: (programNodes at: programIndex) inContext: aDictionary)
		ifFalse: [^false].
	^self
		matchList: matchNodes
		index: matchIndex + 1
		against: programNodes
		index: programIndex + 1
		inContext: aDictionary
]

{ #category : 'accessing' }
ASTProgramNode >> methodNode [
	^parent ifNotNil: [parent methodNode]
]

{ #category : 'accessing' }
ASTProgramNode >> methodOrBlockNode [
	^ parent ifNotNil: [ parent methodOrBlockNode ]
]

{ #category : 'accessing' }
ASTProgramNode >> newSource [
	^self formattedCode
]

{ #category : 'node access' }
ASTProgramNode >> nodeBeforeOffset: anInteger [
	"Return the node at offset `anInteger` or at the first non-separator previous character.
	Ignoring space corresponds to what is the visible current or previous node in IDE."

	| offset |
	offset := anInteger.
	[
	offset > 0 and: [ offset > self source size or: [ (self source at: offset) isSeparator ] ] ]
		whileTrue: [ offset := offset - 1 ].
	^ self nodeForOffset: offset.
]

{ #category : 'node access' }
ASTProgramNode >> nodeForOffset: anInteger [
	"Choose the best node on the specific offset, including comments.
	Because comments can be oustide the span of sourceInterval, we have to visit all children.
	Return nil if anInteger is out of range."

	self childrenAndComments do: [ :each |
		(each nodeForOffset: anInteger) ifNotNil: [ :node | ^ node ] ].

	^ (self sourceInterval includes: anInteger)
		  ifTrue: [ self ]
		  ifFalse: [ nil ]
]

{ #category : 'iterating' }
ASTProgramNode >> nodesDo: aBlock [
	"Evaluate aBlock on self and all chindren nodes, including comments"

	aBlock value: self.
	self childrenAndComments do: [ :each | each nodesDo: aBlock ]
]

{ #category : 'iterating' }
ASTProgramNode >> nodesPostorderDo: aBlock [
	"Like ASTProgramNode>>#nodesDo: but visit children first"

	self childrenAndComments do: [ :each | each nodesPostorderDo: aBlock ].
	aBlock value: self
]

{ #category : 'notice' }
ASTProgramNode >> notices [
	"The notices attached to the receiver"

	^ self propertyAt: #notices ifAbsent: #()
]

{ #category : 'accessing' }
ASTProgramNode >> parent [
	^parent
]

{ #category : 'accessing' }
ASTProgramNode >> parent: aProgramNode [
	parent := aProgramNode
]

{ #category : 'hook' }
ASTProgramNode >> parserClass [
	^ RBParser
]

{ #category : 'copying' }
ASTProgramNode >> postCopy [
	super postCopy.
	properties := properties copy
]

{ #category : 'accessing' }
ASTProgramNode >> precedence [
	^6
]

{ #category : 'printing' }
ASTProgramNode >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self formattedCode;
		nextPut: $)
]

{ #category : 'properties' }
ASTProgramNode >> propertyAt: aKey [
	"Answer the property value associated with aKey."

	^ self propertyAt: aKey ifAbsent: [ self error: 'Property not found' ]
]

{ #category : 'properties' }
ASTProgramNode >> propertyAt: aKey ifAbsent: aBlock [
	"Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock."

	^ properties
		ifNil: [ aBlock value ]
		ifNotNil: [ properties at: aKey ifAbsent: aBlock ]
]

{ #category : 'properties' }
ASTProgramNode >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."

	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : 'accessing' }
ASTProgramNode >> propertyAt: aKey ifPresent: aPresentBlock [
	"Evaluate aPresentBlock with the the property value associated with aKey. Return nil if aKey is not found."

	^ properties ifNotNil: [ properties at: aKey ifPresent: aPresentBlock ifAbsent: [ nil ] ]
]

{ #category : 'properties' }
ASTProgramNode >> propertyAt: aKey ifPresent: aPresentBlock ifAbsent: anAbsentBlock [
	"Answer the property value associated with aKey or, if aKey is found, answer the result of evaluating aPresentBlock, else evaluates anAbsentBlock."

	^ properties ifNil: [ anAbsentBlock value ] ifNotNil: [ properties at: aKey ifPresent: aPresentBlock ifAbsent: anAbsentBlock ]
]

{ #category : 'properties' }
ASTProgramNode >> propertyAt: aKey put: anObject [
	"Set the property at aKey to be anObject. If aKey is not found, create a new entry for aKey and set is value to anObject. Answer anObject."

	^ (properties ifNil: [ properties := SmallDictionary new: 1 ])
		at: aKey put: anObject
]

{ #category : 'testing - matching' }
ASTProgramNode >> recurseInto [
	^false
]

{ #category : 'accessing - meta variable' }
ASTProgramNode >> recurseIntoCharacter [
	^$`
]

{ #category : 'testing' }
ASTProgramNode >> references: aVariableName [
	^self children anySatisfy: [:each | each references: aVariableName]
]

{ #category : 'replacing' }
ASTProgramNode >> removeDeadCode [
	self children do: [:each | each removeDeadCode]
]

{ #category : 'properties' }
ASTProgramNode >> removeProperty: aKey [
	"Remove the property with aKey. Answer the property or raise an error if aKey isn't found."

	^ self removeProperty: aKey ifAbsent: [ self error: 'Property not found' ]
]

{ #category : 'properties' }
ASTProgramNode >> removeProperty: aKey ifAbsent: aBlock [
	"Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock."

	| answer |
	properties ifNil: [ ^ aBlock value ].
	answer := properties removeKey: aKey ifAbsent: aBlock.
	properties isEmpty ifTrue: [ properties := nil ].
	^ answer
]

{ #category : 'replacing' }
ASTProgramNode >> replaceMethodSource: aNode [
	"We are being replaced with aNode -- if possible try to perform an in place edit of the source."

	| method |
	method := self methodNode.
	method ifNotNil: [ method map: self to: aNode ].
	aNode parent: self parent.
	[self replaceSourceWith: aNode] on: Error
		do:
			[:ex |
			self clearReplacements.
			ex return]
]

{ #category : 'replacing' }
ASTProgramNode >> replaceNode: aNode withNode: anotherNode [
	self error: 'I don''t store other nodes'
]

{ #category : 'private - replacing' }
ASTProgramNode >> replaceSourceFrom: aNode [
	self == aNode
		ifFalse: [ self clearReplacements ]
]

{ #category : 'private - replacing' }
ASTProgramNode >> replaceSourceWith: aNode [
	aNode replaceSourceFrom: self
]

{ #category : 'replacing' }
ASTProgramNode >> replaceWith: aNode [
	parent ifNil: [self error: 'This node doesn''t have a parent'].
	self replaceMethodSource: aNode.
	parent replaceNode: self withNode: aNode
]

{ #category : 'enumeration' }
ASTProgramNode >> reverseNodesDo: aBlock [
	self children reverseDo: [ :each | each reverseNodesDo: aBlock ].
	aBlock value: self
]

{ #category : 'querying' }
ASTProgramNode >> selfMessages [
	^(self sendNodes select: [ :node | node isSelfSend ] thenCollect: [ :node | node selector ]) asSet
]

{ #category : 'accessing' }
ASTProgramNode >> sendNodes [
	^self allChildren select: [:each | each isMessage]
]

{ #category : 'accessing' }
ASTProgramNode >> sentMessages [
	^ self children
		inject: Set new
		into: [ :messages :each |
			messages
				addAll: each sentMessages;
				yourself ]
]

{ #category : 'enumeration' }
ASTProgramNode >> size [
	"Hacked to fit collection protocols"

	^1
]

{ #category : 'accessing' }
ASTProgramNode >> source [

	^ parent ifNotNil: [ parent source ]
]

{ #category : 'accessing' }
ASTProgramNode >> sourceCode [

	| interval |
	interval := self sourceInterval.
	interval ifEmpty: [ ^ '' ].

	^ self source
		copyFrom: interval first
		to: interval last
]

{ #category : 'accessing' }
ASTProgramNode >> sourceInterval [
	^ self start to: self stop
]

{ #category : 'accessing' }
ASTProgramNode >> start [
	self subclassResponsibility
]

{ #category : 'accessing - meta variable' }
ASTProgramNode >> statementCharacter [
	^ $.
]

{ #category : 'accessing' }
ASTProgramNode >> statementComments [
	| statementComments |
	statementComments := OrderedCollection withAll: self comments.
	self children do: [:each | statementComments addAll: each statementComments].
	^statementComments asSortedCollection: [:a :b | a start < b start]
]

{ #category : 'querying' }
ASTProgramNode >> statementNode [
	"Return your topmost node that is contained by a sequence node."

	(parent isNil or: [parent isSequence]) ifTrue: [^self].
	^parent statementNode
]

{ #category : 'querying' }
ASTProgramNode >> statementNodeIn: aSequenceBlockOrMethod [
	"Return the statement within aSequenceBlockOrMethod that ultimately contains us,
	skipping any intervening nested blocks.
	
	Note that to avoid repeatedly comparing large chunks of the tree, we use identity for comparison,
	so the provided node must have come from the same parse as the receiver."

	| sequence answer |
	"Retrieve body from method or block node--we need a sequence"
	sequence := aSequenceBlockOrMethod isSequence
		            ifTrue: [ aSequenceBlockOrMethod ]
		            ifFalse: [ aSequenceBlockOrMethod body ].
	answer := self.
	[ answer parent == sequence ] whileFalse: [
		answer := answer parent ifNil: [
			          ^ NotFound signalFor: self in: aSequenceBlockOrMethod ] ].
	^ answer
]

{ #category : 'accessing' }
ASTProgramNode >> statements [
	^ #()
]

{ #category : 'accessing' }
ASTProgramNode >> stop [
	self subclassResponsibility
]

{ #category : 'querying' }
ASTProgramNode >> superMessages [
	^(self sendNodes select: [ :node | node isSuperSend ] thenCollect: [ :node | node selector ]) asSet
]

{ #category : 'querying' }
ASTProgramNode >> tempVariableReadNodes [
		^self variableReadNodes select: [:each | each isTempVariable]
]

{ #category : 'accessing' }
ASTProgramNode >> temporaryVariables [
	^parent ifNil: [#()] ifNotNil: [parent temporaryVariables]
]

{ #category : 'querying' }
ASTProgramNode >> variableDefinitionNodes [
	^ self allChildren select: [ :each |
		  each isVariable and: [ each isDefinition ] ]
]

{ #category : 'querying' }
ASTProgramNode >> variableNodes [

	^ self allChildren select: [ :each |
		  each isVariable and: [ each isDefinition not ] ]
]

{ #category : 'querying' }
ASTProgramNode >> variableReadNodes [
	^self variableNodes select: [:each | each isRead]
]

{ #category : 'querying' }
ASTProgramNode >> variableWriteNodes [
		^self variableNodes select: [:each | each isWrite]
]

{ #category : 'notice' }
ASTProgramNode >> warningNotices [
	"The warnings attached to the receiver"

	^ self notices select: #isWarning
]

{ #category : 'querying' }
ASTProgramNode >> whichNodeIsContainedBy: anInterval [
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	selectedChildren := self children
				select: [:each | each intersectsInterval: anInterval].
	^selectedChildren size == 1
		ifTrue: [selectedChildren first whichNodeIsContainedBy: anInterval]
		ifFalse: [nil]
]

{ #category : 'querying' }
ASTProgramNode >> whoDefines: aName [

	^ (self defines: aName)
		  ifTrue: [ self ]
		  ifFalse: [ parent ifNotNil: [ parent whoDefines: aName ] ]
]

{ #category : 'accessing' }
ASTProgramNode >> withAllParents [
	"return me and all my parents. Topmost parent first, me last
	See discussion in issue https://github.com/pharo-project/pharo/issues/6278"
	^ parent
		ifNil: [ OrderedCollection with: self ]
		ifNotNil: [
			parent withAllParents
				addLast: self;
				yourself ]
]
