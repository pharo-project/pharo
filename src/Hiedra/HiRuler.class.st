"
I represent a ruler of nodes with links.
"
Class {
	#name : #HiRuler,
	#superclass : #Object,
	#instVars : [
		'freeColumnIndexByRow',
		'linkBuilders',
		'values',
		'nodes',
		'links'
	],
	#category : 'Hiedra-Rendering'
}

{ #category : #building }
HiRuler >> build [
	freeColumnIndexByRow := Array new: values size withAll: 1. 
	nodes := OrderedCollection new.
	links := OrderedCollection new.
	
	HiNodesAndLinksIterator new 
		linkBuilders: linkBuilders;
		values: values;
		doWithNodes: [ :node | self processNode: node ]
		doWithLinks: [ :link | self processLink: link ]
]

{ #category : #accessing }
HiRuler >> freeColumnIndexAt: rowIndex [
	^ freeColumnIndexByRow at: rowIndex
]

{ #category : #accessing }
HiRuler >> indentAt: rowIndex [
	freeColumnIndexByRow 
		at: rowIndex 
		put: (freeColumnIndexByRow at: rowIndex) + 1
]

{ #category : #accessing }
HiRuler >> linkBuilders [
	^ linkBuilders
]

{ #category : #accessing }
HiRuler >> linkBuilders: aCollection [
	linkBuilders := aCollection
]

{ #category : #accessing }
HiRuler >> links [
	^ links
]

{ #category : #accessing }
HiRuler >> nodes [
	^ nodes
]

{ #category : #accessing }
HiRuler >> numberOfColumns [
	^ freeColumnIndexByRow 
		ifEmpty: [ 0 ]
		ifNotEmpty: [ freeColumnIndexByRow max ]
]

{ #category : #private }
HiRuler >> processLink: aLink [
	| checkpoints |
	links add: aLink.
	
	checkpoints := OrderedCollection new.

	"add origin checkpoint"
	checkpoints add: aLink origin rulerPoint.
	
	"add checkpoints in the middle"
	aLink intermediateIndices do: [ :rowIndex |
		checkpoints add: (freeColumnIndexByRow at: rowIndex) @ rowIndex.

		self indentAt: rowIndex ].
		
	"add target checkpoint"
	checkpoints add: aLink target rulerPoint.

	"register checkpoints"
	 aLink checkpoints: checkpoints
]

{ #category : #private }
HiRuler >> processNode: node [
	
	nodes add: node.
	node columnIndex: (freeColumnIndexByRow at: node rowIndex).
	self indentAt: node rowIndex.

]

{ #category : #accessing }
HiRuler >> values: aCollection [ 
	values := aCollection
]
