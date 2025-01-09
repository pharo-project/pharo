"
ASTMessageNode is an AST node that represents a message send.

Instance Variables:
	arguments	<SequenceableCollection of: ASTValueNode>	 our argument nodes
	receiver	<ASTValueNode>	the receiver's node
	selector	<Symbol>	the selector we're sending
	keywordsPositions	<IntegerArray | nil>	the positions of the selector keywords
	superOf <Behavior> the current class (for super-send messages)

"
Class {
	#name : 'OCMessageNode',
	#superclass : 'OCValueNode',
	#instVars : [
		'receiver',
		'selector',
		'keywordsPositions',
		'arguments',
		'superOf'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCMessageNode class >> receiver: aValueNode selector: aSelector [
	^self
		receiver: aValueNode
		selector: aSelector
		arguments: #()
]

{ #category : 'instance creation' }
OCMessageNode class >> receiver: aValueNode selector: aSelector arguments: valueNodes [
	^self
		receiver: aValueNode
		selector: aSelector
		keywordsPositions: nil
		arguments: valueNodes
]

{ #category : 'instance creation' }
OCMessageNode class >> receiver: aValueNode selector: aSelector keywordsPositions: positionList arguments: valueNodes [
	^(self new)
		receiver: aValueNode
			selector: aSelector
			keywordsPositions: positionList
			arguments: valueNodes;
		yourself
]

{ #category : 'comparing' }
OCMessageNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	(self receiver = anObject receiver
		and: [self selector = anObject selector]) ifFalse: [^false].
	self arguments
		with: anObject arguments
		do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : 'visiting' }
OCMessageNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitMessageNode: self
]

{ #category : 'accessing' }
OCMessageNode >> argumentPartStrings [
	"Return a collection of string representing the code of the argument."
	^ arguments collect: [ :each | each formattedCode asString
			"strangely enough literal formatted code is not a string" ]
]

{ #category : 'accessing' }
OCMessageNode >> arguments [
	^arguments ifNil: [#()] ifNotNil: [arguments]
]

{ #category : 'accessing' }
OCMessageNode >> arguments: argCollection [
	arguments := argCollection.
	arguments do: [:each | each parent: self]
]

{ #category : 'querying' }
OCMessageNode >> bestNodeFor: anInterval [

	| comments |
	(self intersectsInterval: anInterval) ifFalse: [ ^ nil ].
	(self containedBy: anInterval) ifTrue: [ ^ self ].
	
	self keywordsIntervals do:
			[:each |
			((each rangeIncludes: anInterval first)
				or: [anInterval rangeIncludes:  each first])
					ifTrue: [^self]].
	self children do:
			[:each |
			| node |
			(each intersectsInterval: anInterval) ifTrue: [node := each bestNodeFor: anInterval.
			node ifNotNil: [^node]]].
	comments := self methodNode getCommentsFor: anInterval.
	^comments size ~= 1 ifTrue: [ self ] ifFalse: [ comments first ]
]

{ #category : 'accessing' }
OCMessageNode >> children [
	^(OrderedCollection with: receiver)
		addAll: arguments;
		yourself
]

{ #category : 'matching' }
OCMessageNode >> copyInContext: aDictionary [
	^self class
		receiver: (self receiver copyInContext: aDictionary)
		selector: self selectorNode
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ])
]

{ #category : 'accessing' }
OCMessageNode >> debugHighlightStart [

	^ self  keywordsPositions first
]

{ #category : 'accessing' }
OCMessageNode >> debugHighlightStop [

	^ self stopWithoutParentheses
]

{ #category : 'comparing' }
OCMessageNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [^false].
	((self receiver equalTo: anObject receiver withMapping: aDictionary)
		and: [self selector = anObject selector]) ifFalse: [^false].
	self arguments
		with: anObject arguments
		do: [:first :second | (first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	^true
]

{ #category : 'testing' }
OCMessageNode >> hasBlock [

	^ receiver hasBlock | (arguments anySatisfy: [ :arg | arg hasBlock ])
]

{ #category : 'testing' }
OCMessageNode >> hasSameExitPoint: aBoolean [

	^ (receiver hasSameExitPoint: aBoolean )
	& (arguments allSatisfy: [ :arg | arg hasSameExitPoint: aBoolean ])
]

{ #category : 'comparing' }
OCMessageNode >> hash [
	^ (self receiver hash bitXor: self selector hash) bitXor: (self hashForCollection: self arguments)
]

{ #category : 'testing' }
OCMessageNode >> isAnnotation [
	^ self receiver isAnnotationMark
]

{ #category : 'testing' }
OCMessageNode >> isBinary [
	^(self isUnary or: [self isKeyword]) not
]

{ #category : 'testing' }
OCMessageNode >> isCascaded [
	^parent isNotNil and: [parent isCascade]
]

{ #category : 'private - replacing' }
OCMessageNode >> isContainmentReplacement: aNode [
	^(self mappingFor: self receiver) = aNode
		or: [self arguments anySatisfy: [:each | (self mappingFor: each) = aNode]]
]

{ #category : 'errors' }
OCMessageNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self receiver isFaulty or: [self arguments anySatisfy: [:each | each isFaulty]]
]

{ #category : 'testing' }
OCMessageNode >> isFirstCascaded [
	^self isCascaded and: [parent messages first == self]
]

{ #category : 'testing' }
OCMessageNode >> isHaltNode [

	^ (#( #halt #halt: #haltIf: #haltOnce #haltIfNil #haltOnCount: ) includes: self selector) or: [
		  self receiver isGlobalVariable and: [ self receiver variable value = Halt and: [ (Halt class selectorsInProtocol: #halting) includes: self selector ] ] ]
]

{ #category : 'testing' }
OCMessageNode >> isKeyword [
	^ self selector isKeyword
]

{ #category : 'testing' }
OCMessageNode >> isMessage [
	^true
]

{ #category : 'testing' }
OCMessageNode >> isSelfSend [
	^ self receiver isSelfVariable
]

{ #category : 'testing' }
OCMessageNode >> isSuperSend [
	^ self receiver isSuperVariable
]

{ #category : 'testing' }
OCMessageNode >> isUnary [
	^arguments isEmpty
]

{ #category : 'accessing' }
OCMessageNode >> keywords [
	^ self selector keywords
]

{ #category : 'accessing' }
OCMessageNode >> keywordsIntervals [
	^ self selector keywords
		with: self keywordsPositions
		collect: [ :keyword :start | start to: (start == 0 ifTrue: [ -1 ] ifFalse: [ start + keyword size - 1]) ]
]

{ #category : 'accessing' }
OCMessageNode >> keywordsPositions [
	^keywordsPositions ifNil: [ IntegerArray new: self selector keywords size withAll: 0 ]
]

{ #category : 'accessing' }
OCMessageNode >> keywordsPositions: aPositionsList [
	keywordsPositions := aPositionsList ifNotNil: [:list| list asIntegerArray ]
]

{ #category : 'testing' }
OCMessageNode >> lastIsReturn [
	"Ideally, we want to remove this method so it returns false like the superclass"
	(self isInlineIf or: [ self isInlineIfNil]) ifFalse: [ ^false ].
	arguments size = 1 ifTrue: [ ^false ].
	^ arguments first isBlock and: [ arguments first body lastIsReturn
		and: [ arguments last isBlock and: [ arguments last body lastIsReturn ]]]
]

{ #category : 'accessing' }
OCMessageNode >> leftmostChainReceiver [

	^ self receiver isMessage
		ifTrue: [ self receiver receiver ]
		ifFalse: [ self receiver ]
]

{ #category : 'matching' }
OCMessageNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	self selector = aNode selector ifFalse: [^false].
	(receiver match: aNode receiver inContext: aDictionary) ifFalse: [^false].
	self arguments
		with: aNode arguments
		do: [:first :second | (first match: second inContext: aDictionary) ifFalse: [^false]].
	^true
]

{ #category : 'testing' }
OCMessageNode >> needsParenthesis [
	^parent
		ifNil: [false]
		ifNotNil:
			[self precedence > parent precedence
				or: [self precedence = parent precedence and: [self isUnary not]]]
]

{ #category : 'accessing' }
OCMessageNode >> numArgs [
	^self selector numArgs
]

{ #category : 'copying' }
OCMessageNode >> postCopy [
	super postCopy.
	self receiver: self receiver copy.
	self arguments: (self arguments collect: [ :each | each copy ])
]

{ #category : 'accessing' }
OCMessageNode >> precedence [
	^self isUnary
		ifTrue: [1]
		ifFalse: [self isKeyword ifTrue: [3] ifFalse: [2]]
]

{ #category : 'accessing' }
OCMessageNode >> receiver [
	^receiver
]

{ #category : 'accessing' }
OCMessageNode >> receiver: aValueNode [
	receiver := aValueNode.
	receiver parent: self
]

{ #category : 'initialization' }
OCMessageNode >> receiver: aValueNode selector: aSelector keywordsPositions: positionList arguments: valueNodes [
	aSelector numArgs == valueNodes size
		ifFalse:
			[self error: 'Attempting to assign selector with wrong number of arguments.'].

	self
		receiver: aValueNode;
		arguments: valueNodes;
		selector: aSelector;
		keywordsPositions: positionList
]

{ #category : 'adding-removing' }
OCMessageNode >> removeFromTree [

	self parent removeNode: self
]

{ #category : 'adding-removing' }
OCMessageNode >> removeNode: aNode [

	self replaceNode: aNode withNode: aNode receiver
]

{ #category : 'accessing' }
OCMessageNode >> renameSelector: newSelector andArguments: varNodeCollection [
	self
		arguments: varNodeCollection;
		selector: newSelector
]

{ #category : 'private - replacing' }
OCMessageNode >> replaceContainmentSourceWith: aNode [
	| originalNode needsParenthesis |
	needsParenthesis := aNode hasParentheses not and: [aNode needsParenthesis].
	originalNode := (self mappingFor: self receiver) = aNode
				ifTrue: [self receiver]
				ifFalse: [self arguments detect: [:each | (self mappingFor: each) = aNode]].
	self
		addReplacement: (OCStringReplacement
					replaceFrom: self start
					to: originalNode start - 1
					with: (needsParenthesis ifTrue: ['('] ifFalse: ['']));
		addReplacement: (OCStringReplacement
					replaceFrom: originalNode stop + 1
					to: self stop
					with: (needsParenthesis ifTrue: [')'] ifFalse: ['']))
]

{ #category : 'replacing' }
OCMessageNode >> replaceNode: aNode withNode: anotherNode [
	"If we're inside a cascade node and are changing the receiver, change all the receivers"

	receiver == aNode
		ifTrue:
			[self receiver: anotherNode.
			(parent isNotNil and: [parent isCascade])
				ifTrue: [parent messages do: [:each | each isMessage ifTrue: [ each receiver: anotherNode]]]].
	self arguments: (arguments
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'private - replacing' }
OCMessageNode >> replaceSourceWith: aNode [
	(self isContainmentReplacement: aNode)
		ifTrue: [^self replaceContainmentSourceWith: aNode].
	aNode isMessage ifFalse: [^super replaceSourceWith: aNode].
	^self replaceSourceWithMessageNode: aNode
]

{ #category : 'private - replacing' }
OCMessageNode >> replaceSourceWithMessageNode: aNode [
	| isBinaryToKeyword newSelectorParts  oldkeywordsIntervals|
	self numArgs = aNode numArgs ifFalse: [^super replaceSourceWith: aNode].
	self arguments with: aNode arguments
		do: [:old :new | (self mappingFor: old) = new ifFalse: [^super replaceSourceWith: aNode]].
	(self mappingFor: self receiver) = aNode receiver
		ifFalse:
			[(self receiver isVariable and: [aNode receiver isVariable])
				ifFalse:
					[^super replaceSourceWith: aNode].
			self addReplacement:
				(OCStringReplacement
					replaceFrom: self receiver start
					to: self receiver stop
					with: aNode receiver name)].
	(isBinaryToKeyword := self isBinary and: [aNode isKeyword])
		ifTrue:
			[(self hasParentheses not and: [self parent precedence <= aNode precedence])
				ifTrue:
					[self
						addReplacement: (OCStringReplacement
									replaceFrom: self start
									to: self start - 1
									with: '(');
						addReplacement: (OCStringReplacement
									replaceFrom: self stop + 1
									to: self stop
									with: ')')]].

	newSelectorParts := aNode selectorParts.
	oldkeywordsIntervals := self keywordsIntervals.

	self selectorParts keysAndValuesDo: [:index :oldPart|
			(newSelectorParts at: index) ~= oldPart
				ifTrue:
					[self addReplacement: (OCStringReplacement
								replaceFrom: (oldkeywordsIntervals at: index) first
								to: (oldkeywordsIntervals at: index) last
								with: ((isBinaryToKeyword
										and: [(self source at: (oldkeywordsIntervals at: index) first - 1) isSeparator not])
											ifTrue: [' ' , newSelectorParts at: index]
											ifFalse: [newSelectorParts at: index]))]]
]

{ #category : 'accessing' }
OCMessageNode >> selector [
	^ selector value
]

{ #category : 'accessing' }
OCMessageNode >> selector: aSelector [
	keywordsPositions := nil.
	selector := aSelector isString
		ifTrue: [ OCSelectorNode value: aSelector ]
		ifFalse: [ aSelector ].
	selector parent: self
]

{ #category : 'accessing' }
OCMessageNode >> selectorAndArgumentNames [
	"Returns the selector and argument names portion of a method as a string"

	^ self arguments
		ifEmpty: [ self keywords first ]
		ifNotEmpty: [
			(String streamContents: [ :st |
				self selectorParts
			 		with: self arguments
					do: [:sel :arg |
							st
								<< sel asString;
								<< ' ';
								<< arg formattedCode;
								<< ' ']]) allButLast ]
]

{ #category : 'accessing' }
OCMessageNode >> selectorInterval [
	| positions |
	positions := self keywordsIntervals.
	^positions first first to: positions last last
]

{ #category : 'accessing' }
OCMessageNode >> selectorNode [
	^selector
]

{ #category : 'accessing' }
OCMessageNode >> selectorParts [
	"Returns a collection containing all the selector composing a message"

	^ self keywords collect: [ :each | each asSymbol]
]

{ #category : 'accessing' }
OCMessageNode >> sentMessages [
	^ super sentMessages
		add: self selector;
		yourself
]

{ #category : 'accessing' }
OCMessageNode >> start: newStart [
	| oldStart |

	oldStart := self start.
 	keywordsPositions := keywordsPositions collect: [ :each | each - oldStart + newStart ].
	receiver start: newStart
]

{ #category : 'accessing' }
OCMessageNode >> startWithoutParentheses [
	^receiver start
]

{ #category : 'accessing' }
OCMessageNode >> stopWithoutParentheses [
	^arguments isEmpty
		ifTrue: [self keywordsIntervals first last]
		ifFalse: [arguments last stop]
]

{ #category : 'accessing' }
OCMessageNode >> superOf [

	^ superOf
]

{ #category : 'accessing' }
OCMessageNode >> superOf: anObject [

	superOf := anObject
]
