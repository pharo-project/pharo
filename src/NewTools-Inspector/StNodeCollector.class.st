Class {
	#name : #StNodeCollector,
	#superclass : #Object,
	#instVars : [
		'object'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #'instance creation' }
StNodeCollector class >> for: anObject [

	^ self basicNew 
		initializeObject: anObject;
		yourself
]

{ #category : #building }
StNodeCollector >> collectContextNodes [
	^ OrderedCollection
		with: (StInspectorThisContextNode hostObject: self object context)
		with: (StInspectorStackTopNode hostObject: self object context)
		with: (StDebugSessionInspectorNode hostObject: self object session)
]

{ #category : #building }
StNodeCollector >> collectNodes [

	^ 
	self indexedNodes,	
	self slotNodes
]

{ #category : #building }
StNodeCollector >> collectTempsNodes [
	^self tempNodesForContext: object context
]

{ #category : #private }
StNodeCollector >> indexedNodes [
	| nodes indexableDisplayLimit top bottom topLimit bottomLimit |
	
	nodes := OrderedCollection new.
	
	indexableDisplayLimit := StInspector indexableDisplayLimit.        
	top := 1.
	bottom := self object basicSize.
	topLimit    := indexableDisplayLimit min: bottom.
	bottomLimit := indexableDisplayLimit max: (bottom - indexableDisplayLimit).

	top to: topLimit do: [ :index | 
		nodes add: (StInspectorIndexedNode 
			hostObject: self object
			index: index) ].
	
	bottomLimit + 1 to: bottom do: [ :index |
		nodes add: (StInspectorIndexedNode 
			hostObject: self object
			index: index) ].
	
	^ nodes
]

{ #category : #initialization }
StNodeCollector >> initializeObject: anObject [

	self initialize.
	object := anObject
]

{ #category : #accessing }
StNodeCollector >> object [ 
	
	^ object
]

{ #category : #accessing }
StNodeCollector >> objectClass [ 
	
	^ self object class
]

{ #category : #private }
StNodeCollector >> slotNodes [

	^ self objectClass allSlots collect: [ :each | 
			StInspectorSlotNode 
				hostObject: self object 
				slot: each ]
]

{ #category : #building }
StNodeCollector >> tempNodesForContext: aContext [
	^ aContext tempNames collect: [ :name | 
		  (StInspectorTempNode hostObject: aContext) tempVariable:
			  (aContext temporaryVariableNamed: name) ]
]
