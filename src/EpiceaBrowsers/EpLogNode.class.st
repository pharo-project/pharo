"
I am a node in a tree of Epicea logs, in a particular refresh of the browser. The nodes of such tree are Epicea files and an edge between a node A and another B exist when A contains an entry whose prior node is in B.
"
Class {
	#name : #EpLogNode,
	#superclass : #Object,
	#instVars : [
		'referencedNodes'
	],
	#category : #'EpiceaBrowsers-UI-Graph'
}

{ #category : #accessing }
EpLogNode >> globalName [

	^ self subclassResponsibility
]

{ #category : #initialization }
EpLogNode >> initialize [
	super initialize.
	
	referencedNodes := Set new.

]

{ #category : #testing }
EpLogNode >> isMonitor [

	^ false
]

{ #category : #accessing }
EpLogNode >> log [

	^ self subclassResponsibility
]

{ #category : #accessing }
EpLogNode >> modificationTime [

	^ self subclassResponsibility
]

{ #category : #initialization }
EpLogNode >> populateReferencedNodesWith: nodes [

	self referencedGlobalNames do: [ :referencedGlobalName |
		nodes
			detect: [:each | each globalName = referencedGlobalName ]
			ifFound: [ :referencedNode | referencedNodes add: referencedNode ] ] 

]

{ #category : #accessing }
EpLogNode >> referencedGlobalNames [

	| firstEntry priorReference |
	firstEntry := self log firstEntryIfAbsent: [ ^ self referencedGlobalNamesIfEmptyLog ].
	priorReference := firstEntry tags at: EpLog priorReferenceKey ifAbsent: [ ^ #() ].
	priorReference isNull ifTrue: [ ^ #() ].
	
	"Could happen when monitor's ombu file is removed externally"
	priorReference globalName = self globalName ifTrue: [ ^ #() ].

	^ { priorReference globalName }
]

{ #category : #accessing }
EpLogNode >> referencedGlobalNamesIfEmptyLog [

	^ #()
]

{ #category : #accessing }
EpLogNode >> referencedNodes [
	"Answer the referenced nodes, that were populated previously by #populateReferencedNodesWith:"
	
	^ referencedNodes
]
