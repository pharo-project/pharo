"
I know how to build rulers.
"
Class {
	#name : #HiRulerBuilder,
	#superclass : #HiBuilder,
	#instVars : [
		'rowSize',
		'columnSize',
		'nodeBuilder',
		'linkBuilders',
		'values'
	],
	#category : #'Hiedra-Builders'
}

{ #category : #'accessing - building API' }
HiRulerBuilder >> addLinkBuilder: aBlockClosure [ 
	
	| newLink |
	newLink := HiLinkBuilder new.
	aBlockClosure value: newLink.
	linkBuilders add: newLink.
]

{ #category : #building }
HiRulerBuilder >> athensSurface [
	| athensSurface ruler |
	ruler := HiRuler new
		values: values;
		linkBuilders: linkBuilders;
		build;
		yourself.

	"FIX: it's (ruler numberOfColumns + 1) because of the left margin"
	athensSurface := AthensCairoSurface extent: 
		((ruler numberOfColumns + 1) * self columnSize) @ (self numberOfRows * self rowSize).
		
	athensSurface drawDuring: [ :athensCanvas | 

		"apply left margin"
		athensCanvas pathTransform
			translateX: self columnSize
			Y: 0.
					
		(HiRulerRenderer 
			model: self 
			athensCanvas: athensCanvas
			ruler: ruler) 
			render ].

	^ athensSurface
]

{ #category : #'accessing - rendering' }
HiRulerBuilder >> bottomOfRowIndex: rowIndex [
	^ rowIndex * self rowSize
]

{ #category : #'accessing - rendering' }
HiRulerBuilder >> centerOfColumnIndex: columnIndex [
	^ (self leftOfColumnIndex: columnIndex) + (self columnSize // 2)
]

{ #category : #'accessing - rendering' }
HiRulerBuilder >> centerOfRowIndex: rowIndex [
	^ (self topOfRowIndex: rowIndex) + (self rowSize // 2)
]

{ #category : #'accessing - building API' }
HiRulerBuilder >> columnSize [
	^ columnSize
]

{ #category : #'accessing - building API' }
HiRulerBuilder >> columnSize: anInteger [
	columnSize := anInteger
]

{ #category : #building }
HiRulerBuilder >> form [
	| surface form |
	surface := self athensSurface.

	form := Form 
		extent: (surface width @ surface height) 
		depth: 32.

	form getCanvas 
		drawImage: surface asForm 
		at: 0@0.

	^ form
]

{ #category : #initialization }
HiRulerBuilder >> initialize [
	super initialize.
	
	rowSize := 25.
	columnSize := rowSize // 2.
	nodeBuilder := HiNodeBuilder new.
	linkBuilders := OrderedCollection new.
]

{ #category : #'accessing - rendering' }
HiRulerBuilder >> leftOfColumnIndex: columnIndex [
	^ (columnIndex - 1) * self columnSize
]

{ #category : #'accessing - rendering' }
HiRulerBuilder >> linkBuilders [
	^ linkBuilders
]

{ #category : #'accessing - building API' }
HiRulerBuilder >> nodeBuilder [
	^ nodeBuilder
]

{ #category : #'accessing - rendering' }
HiRulerBuilder >> numberOfRows [
	^ values size
]

{ #category : #'accessing - building API' }
HiRulerBuilder >> rowSize [
	^ rowSize
]

{ #category : #'accessing - building API' }
HiRulerBuilder >> rowSize: anInteger [
	rowSize := anInteger
]

{ #category : #'accessing - rendering' }
HiRulerBuilder >> topOfRowIndex: rowIndex [
	^ (rowIndex - 1) * self rowSize
]

{ #category : #'accessing - building API' }
HiRulerBuilder >> values: aCollection [ 
	values := aCollection
]
