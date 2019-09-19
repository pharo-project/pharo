"
I am a node to represent nothing. 

I can be useful for Pharo rewriters to represent a rewrite that should nemove the matching nodes.
"
Class {
	#name : #RBEmptyNode,
	#superclass : #RBProgramNode,
	#instVars : [
		'start',
		'periods',
		'temporaries'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBEmptyNode class >> fillFrom: aSequenceNode [
	^ self new
		fillFrom: aSequenceNode;
		yourself
]

{ #category : #comparing }
RBEmptyNode >> = anObject [
	"Can't send = to the temporaries and statements collection since they might change from arrays to OCs"

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	self temporaries size = anObject temporaries size ifFalse: [ ^ false ].
	self temporaries with: anObject temporaries do: [ :first :second | first = second ifFalse: [ ^ false ] ].

	^ true
]

{ #category : #visiting }
RBEmptyNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitEmptyNode: self
]

{ #category : #accessing }
RBEmptyNode >> allDefinedVariables [
	^ self temporaryNames asOrderedCollection
		addAll: super allDefinedVariables;
		yourself
]

{ #category : #accessing }
RBEmptyNode >> allTemporaryVariables [
	^ self temporaryNames asOrderedCollection
		addAll: super allTemporaryVariables;
		yourself
]

{ #category : #accessing }
RBEmptyNode >> children [
	^ OrderedCollection withAll: self temporaries
]

{ #category : #matching }
RBEmptyNode >> copyInContext: aDictionary [
	^ self class new
		temporaries: (self copyList: self temporaries inContext: aDictionary);
		yourself
]

{ #category : #testing }
RBEmptyNode >> defines: aName [
	^ temporaries anySatisfy: [ :each | each name = aName ]
]

{ #category : #testing }
RBEmptyNode >> directlyUses: aNode [
	^ false
]

{ #category : #'instance creation' }
RBEmptyNode >> fillFrom: aSequenceNode [
	self
		start: aSequenceNode start;
		periods: aSequenceNode periods;
		temporaries: aSequenceNode temporaries;
		comments: aSequenceNode comments;
		parent: aSequenceNode parent
]

{ #category : #comparing }
RBEmptyNode >> hash [
	^ self hashForCollection: self temporaries
]

{ #category : #testing }
RBEmptyNode >> isEmptyNode [
	^ true
]

{ #category : #testing }
RBEmptyNode >> isFaulty [
	^ true
]

{ #category : #replacing }
RBEmptyNode >> match: aNode inContext: aDictionary [
	self class = aNode class ifFalse: [ ^ false ].
	^ self matchList: temporaries against: aNode temporaries inContext: aDictionary
]

{ #category : #replacing }
RBEmptyNode >> methodComments [
	| methodComments |
	methodComments := OrderedCollection withAll: self comments.
	temporaries do: [ :each | methodComments addAll: each comments ].
	(parent notNil and: [ parent isBlock ]) ifTrue: [ parent arguments do: [ :each | methodComments addAll: each comments ] ].
	^ methodComments asSortedCollection: [ :a :b | a start < b start ]
]

{ #category : #accessing }
RBEmptyNode >> periods [
	^ periods
]

{ #category : #accessing }
RBEmptyNode >> periods: anObject [
	periods := anObject
]

{ #category : #copying }
RBEmptyNode >> postCopy [
	super postCopy.
	self temporaries: (self temporaries collect: [ :each | each copy ])
]

{ #category : #replacing }
RBEmptyNode >> replaceNode: aNode withNode: anotherNode [
	self temporaries: (temporaries collect: [ :each | each == aNode
		ifTrue: [ anotherNode ]
		ifFalse: [ each ] ])
]

{ #category : #accessing }
RBEmptyNode >> start [
	^ start
]

{ #category : #accessing }
RBEmptyNode >> start: anObject [
	start := anObject
]

{ #category : #accessing }
RBEmptyNode >> stop [
	^ {(temporaries isEmpty ifTrue: [ 0 ] ifFalse: [ temporaries last stop ]) . (periods isEmpty ifTrue: [ 0 ] ifFalse: [ periods last ])} max
]

{ #category : #accessing }
RBEmptyNode >> temporaries [
	^ temporaries
]

{ #category : #accessing }
RBEmptyNode >> temporaries: anObject [
	temporaries := anObject
]

{ #category : #accessing }
RBEmptyNode >> temporaryVariables [
	^ super temporaryVariables asOrderedCollection
		addAll: self temporaryNames;
		yourself
]
