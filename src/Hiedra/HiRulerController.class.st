"
I'm the core of Hiedra. See my users to understand me.
"
Class {
	#name : #HiRulerController,
	#superclass : #Object,
	#instVars : [
		'rulerBuilder',
		'rulerForm',
		'indexesByNode',
		'nodes',
		'treeModel'
	],
	#category : #Hiedra-Spec
}

{ #category : #private-refreshing }
HiRulerController >> buildIndexesByNode [
	indexesByNode := IdentityDictionary new.
	nodes withIndexDo: [ :aNode :index |
		indexesByNode 
			at: aNode 
			put: index ]
]

{ #category : #private-refreshing }
HiRulerController >> buildRulerForm [
	rulerForm := 
		rulerBuilder
			values: nodes;
			form
]

{ #category : #accessing }
HiRulerController >> indexOfNode: aNode [
	^ indexesByNode at: aNode
]

{ #category : #initialization }
HiRulerController >> initialize [
	super initialize.
	self initializeRulerBuilder.
	nodes := #().
]

{ #category : #initialization }
HiRulerController >> initializeRulerBuilder [
	rulerBuilder := HiRulerBuilder new.
]

{ #category : #convenience }
HiRulerController >> newTreeColumnModel [
	"Answer a TreeColumnModel ready to be used in a TreeModel."
	
	^ TreeColumnModel new	
		displayBlock: [ :item | self rowMorphFor: item content ];
		yourself.	
]

{ #category : #accessing }
HiRulerController >> nodes [
	^ nodes
]

{ #category : #accessing }
HiRulerController >> preferredWidth [
	^ rulerForm width
]

{ #category : #private-refreshing }
HiRulerController >> refreshRuler [
	self buildIndexesByNode.
	self buildRulerForm.
]

{ #category : #accessing }
HiRulerController >> rowMorphFor: aMorphTreeNode [
	^ HiRulerLazyMorph model: self key: aMorphTreeNode withoutListWrapper
]

{ #category : #accessing }
HiRulerController >> rulerBuilder [
	^ rulerBuilder
]

{ #category : #accessing }
HiRulerController >> rulerForm [
	^ rulerForm
]

{ #category : #accessing }
HiRulerController >> rulerMorphBoundsFor: aNode [
	| nodeIndex |
	"hack: sync with tree if needed"
	(nodes includes: aNode) ifFalse: [ self updateFromTree ].
	
	nodeIndex := self indexOfNode: aNode.

	^ 0 @ ((nodeIndex - 1) * rulerBuilder rowSize) corner: rulerForm width @ (nodeIndex * rulerBuilder rowSize)
]

{ #category : #accessing }
HiRulerController >> treeModel [
	^ treeModel
]

{ #category : #accessing }
HiRulerController >> treeModel: anObject [
	treeModel := anObject
]

{ #category : #private-refreshing }
HiRulerController >> updateFromTree [
	self values: treeModel roots
]

{ #category : #accessing }
HiRulerController >> values: aCollection [ 
	nodes := aCollection.
	
	self refreshRuler.
]
