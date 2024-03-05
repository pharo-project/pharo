"
I am a behavior that can be added to debug points. 
I maintain a chain of debug points. When a debug point in the chain is hit I deactivate it and activate the next debug point in the chain.
"
Class {
	#name : 'ChainBehavior',
	#superclass : 'DebugPointBehavior',
	#classVars : [
		'Chains'
	],
	#category : 'DebugPoints-Behaviors',
	#package : 'DebugPoints',
	#tag : 'Behaviors'
}

{ #category : 'accessing' }
ChainBehavior class >> allChains [
	^Chains ifNil: [ Chains := Dictionary new. ]
]

{ #category : 'accessing' }
ChainBehavior class >> childFor: aDebugPoint [
	^self allChains at: aDebugPoint ifAbsent: [ ^nil ]
]

{ #category : 'accessing' }
ChainBehavior class >> parentFor: aDebugPoint [
	^self allChains keyAtValue: aDebugPoint ifAbsent: [^nil].
]

{ #category : 'removing' }
ChainBehavior class >> removeChild: aDebugPoint [

	self allChains
		removeKey: (self allChains
				 keyAtValue: aDebugPoint
				 ifAbsent: [ nil ])
		ifAbsent: [  ]
]

{ #category : 'accessing' }
ChainBehavior class >> rootFor: aDebugPoint [

	| parent |
	parent := self allChains 
		          keyAtValue: aDebugPoint
		          ifAbsent: [ ^ aDebugPoint ].
	^ self rootFor: parent
]

{ #category : 'private' }
ChainBehavior >> checkCircularChain: aDebugPoint withParent: aParent [
	"should return true if there is a circular dependency so that the child will not be added"
	| parent |
	(aParent = aDebugPoint) ifTrue: [ 
		self circularChainPopup.
		^true ].
	
	parent := (self class parentFor: aParent) ifNil: [ ^false ].
	
	^self checkCircularChain: aDebugPoint withParent: parent.


]

{ #category : 'accessing' }
ChainBehavior >> child [
	^self class childFor: self debugPoint.

]

{ #category : 'private' }
ChainBehavior >> circularChainPopup [
		SpInformDialog new title: 'Circular Dependency';label: 'Adding this child would result in a circular chain, which is not allowed.'; openModal.

]

{ #category : 'execution' }
ChainBehavior >> execute [
	self child ifNotNil: [:child | child enabled: true].
	self debugPoint enabled: false.
	^true
]

{ #category : 'accessing' }
ChainBehavior >> parent [
	^self class parentFor: self debugPoint.
]

{ #category : 'API' }
ChainBehavior >> putChild: aDebugPoint [

	(self checkCircularChain: aDebugPoint withParent: self debugPoint)
		ifTrue: [ ^ self ].
	"check if there is already a parent or smth"
	self class removeChild: aDebugPoint.
	self class allChains at: self debugPoint put: aDebugPoint.
	"When a chain is modified, the chain is reset"
	self resetChain.
	DebugPointManager notifyDebugPointChanged: aDebugPoint
]

{ #category : 'cleanup' }
ChainBehavior >> remove [ 
	"removing element as child and as parent of other debug points"
	self class allChains removeKey: self debugPoint ifAbsent: [].
	self class removeChild: self debugPoint.

]

{ #category : 'API' }
ChainBehavior >> resetChain [
	| root |
	root := (self class rootFor: self debugPoint).
	root enabled: true.
	self resetSubChain: root 
	
]

{ #category : 'private' }
ChainBehavior >> resetSubChain: aDebugPoint [
	| child |
	child := 	self class allChains at: aDebugPoint ifAbsent: [ ^nil. ].
	child ifNotNil: [ child enabled: false.
		self resetSubChain: child].

	
]
