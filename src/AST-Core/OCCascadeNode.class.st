"
ASTCascadeNode is an AST node for cascaded messages (e.g., ""self print1 ; print2"").

Instance Variables:
	messages	<SequenceableCollection of: ASTMessageNode>	the messages 
	semicolons	<SequenceableCollection of: Integer>	positions of the ; between messages


"
Class {
	#name : 'OCCascadeNode',
	#superclass : 'OCValueNode',
	#instVars : [
		'messages',
		'semicolons'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCCascadeNode class >> messages: messageNodes [
	^self new messages: messageNodes
]

{ #category : 'instance creation' }
OCCascadeNode class >> messages: messageNodes semicolons: integerCollection [
	^self new messages: messageNodes semicolons: integerCollection
]

{ #category : 'comparing' }
OCCascadeNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self messages size = anObject messages size ifFalse: [^false].
	self messages with: anObject messages do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : 'visiting' }
OCCascadeNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitCascadeNode: self
]

{ #category : 'querying' }
OCCascadeNode >> bestNodeFor: anInterval [

	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [ ^ nil ].
	(self containedBy: anInterval) ifTrue: [ ^ self ].
	
	messages
		reverseDo: [:each | (each containedBy: anInterval) ifTrue: [ ^ each ] ].
		
	selectedChildren := 
		messages
				collect: [:each | each bestNodeFor: anInterval]
				thenReject: [ :each | each isNil ].
	^ selectedChildren detect: [ :each | true ] ifNone: [ nil ]
]

{ #category : 'accessing' }
OCCascadeNode >> children [
	^ messages
]

{ #category : 'matching' }
OCCascadeNode >> copyInContext: aDictionary [
	^ self class new
		messages: (self copyList: self messages inContext: aDictionary);
		yourself
]

{ #category : 'comparing' }
OCCascadeNode >> equalTo: anObject withMapping: aDictionary [
	self class = anObject class ifFalse: [^false].
	self messages size = anObject messages size ifFalse: [^false].
	self messages
		with: anObject messages
		do: [:first :second | (first equalTo: second withMapping: aDictionary) ifFalse: [^false]].
	^true
]

{ #category : 'testing' }
OCCascadeNode >> hasBlock [

	"They all have same receiver so we only check with the first one."
	^ messages first hasBlock
]

{ #category : 'comparing' }
OCCascadeNode >> hash [
	^ self hashForCollection: self messages
]

{ #category : 'testing' }
OCCascadeNode >> isCascade [
	^true
]

{ #category : 'testing' }
OCCascadeNode >> isEssentialChild: aNode [
	"Removing a message in a cascade is not considered as a problem from a syntatic point of view."

	^ false
]

{ #category : 'errors' }
OCCascadeNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self messages anySatisfy: [:each | each isFaulty ]
]

{ #category : 'testing' }
OCCascadeNode >> isUsingAsReturnValue: aNode [
	^messages last = aNode and: [self isUsedAsReturnValue]
]

{ #category : 'accessing' }
OCCascadeNode >> leftmostChainReceiver [

	^ self receiver isMessage
		ifTrue: [ self receiver receiver ]
		ifFalse: [ self receiver ]
]

{ #category : 'matching' }
OCCascadeNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^self
		matchList: messages
		against: aNode messages
		inContext: aDictionary
]

{ #category : 'accessing' }
OCCascadeNode >> messages [
	^messages
]

{ #category : 'accessing' }
OCCascadeNode >> messages: messageNodeCollection [
	messages := messageNodeCollection.
	messages do: [:each | each parent: self]
]

{ #category : 'initialization' }
OCCascadeNode >> messages: messageNodes semicolons: integerCollection [
	self messages: messageNodes.
	semicolons := integerCollection
]

{ #category : 'testing' }
OCCascadeNode >> needsParenthesis [
	^parent
		ifNil: [false]
		ifNotNil: [self precedence > parent precedence]
]

{ #category : 'copying' }
OCCascadeNode >> postCopy [
	super postCopy.
	self messages: (self messages collect: [ :each | each copy ])
]

{ #category : 'accessing' }
OCCascadeNode >> precedence [
	^4
]

{ #category : 'accessing' }
OCCascadeNode >> receiver [
	^self messages first receiver
]

{ #category : 'adding-removing' }
OCCascadeNode >> removeNode: aNode [

	self messages remove: aNode ifAbsent: [ ]
]

{ #category : 'replacing' }
OCCascadeNode >> replaceNode: aNode withNode: anotherNode [
	self messages: (messages
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'accessing - token' }
OCCascadeNode >> semicolons [
	^ semicolons
]

{ #category : 'accessing - token' }
OCCascadeNode >> semicolons: anArray [
	semicolons := anArray
]

{ #category : 'accessing' }
OCCascadeNode >> startWithoutParentheses [
	^messages first start
]

{ #category : 'accessing' }
OCCascadeNode >> statementComments [
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
OCCascadeNode >> stopWithoutParentheses [
	^messages last stop
]

{ #category : 'querying' }
OCCascadeNode >> whichNodeIsContainedBy: anInterval [

	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [ ^ nil ].
	(self containedBy: anInterval) ifTrue: [ ^ self ].
	messages
		reverseDo: [:each | (each containedBy: anInterval) ifTrue: [ ^ each ]].
	selectedChildren := messages
				collect: [:each | each whichNodeIsContainedBy: anInterval]
				thenReject: [:each | each isNil] .
	^ selectedChildren detect: [:each | true ] ifNone: [ nil ]
]
