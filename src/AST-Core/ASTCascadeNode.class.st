"
RBCascadeNode is an AST node for cascaded messages (e.g., ""self print1 ; print2"").

Instance Variables:
	messages	<SequenceableCollection of: RBMessageNode>	the messages 
	semicolons	<SequenceableCollection of: Integer>	positions of the ; between messages


"
Class {
	#name : 'ASTCascadeNode',
	#superclass : 'ASTValueNode',
	#instVars : [
		'messages',
		'semicolons'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
ASTCascadeNode class >> messages: messageNodes [
	^self new messages: messageNodes
]

{ #category : 'instance creation' }
ASTCascadeNode class >> messages: messageNodes semicolons: integerCollection [
	^self new messages: messageNodes semicolons: integerCollection
]

{ #category : 'comparing' }
ASTCascadeNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self messages size = anObject messages size ifFalse: [^false].
	self messages with: anObject messages do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : 'visiting' }
ASTCascadeNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitCascadeNode: self
]

{ #category : 'querying' }
ASTCascadeNode >> bestNodeFor: anInterval [
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [ ^nil ] .
	(self containedBy: anInterval) ifTrue: [^self].
	messages
		reverseDo: [:each | (each containedBy: anInterval) ifTrue: [^each]].
	selectedChildren := (messages
				collect: [:each | each bestNodeFor: anInterval])
					reject: [:each | each isNil].
	^selectedChildren detect: [:each | true] ifNone: [nil]
]

{ #category : 'accessing' }
ASTCascadeNode >> children [
	^ messages
]

{ #category : 'matching' }
ASTCascadeNode >> copyInContext: aDictionary [
	^ self class new
		messages: (self copyList: self messages inContext: aDictionary);
		yourself
]

{ #category : 'comparing' }
ASTCascadeNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [^false].
	self messages size = anObject messages size ifFalse: [^false].
	self messages
		with: anObject messages
		do: [:first :second | (first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	^true
]

{ #category : 'testing' }
ASTCascadeNode >> hasBlock [

	"They all have same receiver so we only check with the first one."
	^ messages first hasBlock
]

{ #category : 'comparing' }
ASTCascadeNode >> hash [
	^ self hashForCollection: self messages
]

{ #category : 'testing' }
ASTCascadeNode >> isCascade [
	^true
]

{ #category : 'testing' }
ASTCascadeNode >> isEssentialChild: aNode [
	"Removing a message in a cascade is not considered as a problem from a syntatic point of view."

	^ false
]

{ #category : 'errors' }
ASTCascadeNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self messages anySatisfy: [:each | each isFaulty ]
]

{ #category : 'testing' }
ASTCascadeNode >> isUsingAsReturnValue: aNode [
	^messages last = aNode and: [self isUsedAsReturnValue]
]

{ #category : 'accessing' }
ASTCascadeNode >> leftmostChainReceiver [

	^ self receiver isMessage
		ifTrue: [ self receiver receiver ]
		ifFalse: [ self receiver ]
]

{ #category : 'matching' }
ASTCascadeNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^self
		matchList: messages
		against: aNode messages
		inContext: aDictionary
]

{ #category : 'accessing' }
ASTCascadeNode >> messages [
	^messages
]

{ #category : 'accessing' }
ASTCascadeNode >> messages: messageNodeCollection [
	messages := messageNodeCollection.
	messages do: [:each | each parent: self]
]

{ #category : 'initialization' }
ASTCascadeNode >> messages: messageNodes semicolons: integerCollection [
	self messages: messageNodes.
	semicolons := integerCollection
]

{ #category : 'testing' }
ASTCascadeNode >> needsParenthesis [
	^parent
		ifNil: [false]
		ifNotNil: [self precedence > parent precedence]
]

{ #category : 'copying' }
ASTCascadeNode >> postCopy [
	super postCopy.
	self messages: (self messages collect: [ :each | each copy ])
]

{ #category : 'accessing' }
ASTCascadeNode >> precedence [
	^4
]

{ #category : 'accessing' }
ASTCascadeNode >> receiver [
	^self messages first receiver
]

{ #category : 'adding-removing' }
ASTCascadeNode >> removeNode: aNode [

	self messages remove: aNode ifAbsent: [ ]
]

{ #category : 'replacing' }
ASTCascadeNode >> replaceNode: aNode withNode: anotherNode [
	self messages: (messages
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'accessing - token' }
ASTCascadeNode >> semicolons [
	^ semicolons
]

{ #category : 'accessing - token' }
ASTCascadeNode >> semicolons: anArray [
	semicolons := anArray
]

{ #category : 'accessing' }
ASTCascadeNode >> startWithoutParentheses [
	^messages first start
]

{ #category : 'accessing' }
ASTCascadeNode >> statementComments [
	| statementComments |
	statementComments := OrderedCollection withAll: self comments.
	statementComments addAll: messages first receiver statementComments.
	messages do:
			[:each |
			each arguments
				do: [:arg | statementComments addAll: arg statementComments]].
	^statementComments asSortedCollection: [:a :b | a start < b start]
]

{ #category : 'accessing' }
ASTCascadeNode >> stopWithoutParentheses [
	^messages last stop
]

{ #category : 'querying' }
ASTCascadeNode >> whichNodeIsContainedBy: anInterval [
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	messages
		reverseDo: [:each | (each containedBy: anInterval) ifTrue: [^each]].
	selectedChildren := (messages
				collect: [:each | each whichNodeIsContainedBy: anInterval])
					reject: [:each | each isNil].
	^selectedChildren detect: [:each | true] ifNone: [nil]
]
