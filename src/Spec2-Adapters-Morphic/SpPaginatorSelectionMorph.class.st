"
A morph to being used as a selection area in a `SpPaginatorMorph`.
It can be dragged and resized.
"
Class {
	#name : #SpPaginatorSelectionMorph,
	#superclass : #Morph,
	#instVars : [
		'lastMouseX',
		'lastX',
		'dragFinishedAction',
		'draggingAction',
		'minimumWidth'
	],
	#category : #'Spec2-Adapters-Morphic-Paginator'
}

{ #category : #initialization }
SpPaginatorSelectionMorph >> addResizingEdges [

	"left resizer"
	self addMorphBack: (SpPaginatorEdgeGripMorph new 
		beLeft;
		target: self;
		yourself).

	"A transparent morph to fill the space"
	self addMorphBack: (Morph new 
		color: Color transparent;
		height: 0;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself).

	"right resizer"
	self addMorphBack: (SpPaginatorEdgeGripMorph new 
		beRight;
		target: self; 
		yourself)
]

{ #category : #private }
SpPaginatorSelectionMorph >> announceDragFinished [

	dragFinishedAction ifNil: [ ^ self ].
	dragFinishedAction cull: self left
]

{ #category : #testing }
SpPaginatorSelectionMorph >> canResizeTo: xNumber direction: direction [

	(xNumber between: self owner left and: self owner right) ifFalse: [ ^ false ].
	(xNumber between: self left and: self right) ifTrue: [ 
		(direction = #left) ifTrue: [ ^ (self right - xNumber) > minimumWidth ]. 		
		(direction = #right) ifTrue: [ ^ (xNumber - self left) > minimumWidth ] ].
		
	^ true
]

{ #category : #initialization }
SpPaginatorSelectionMorph >> defaultColor [
	
	^ self theme menuSelectionColor alpha: 0.8
]

{ #category : #'event handling' }
SpPaginatorSelectionMorph >> handlesMouseDown: anEvent [

	^ true
]

{ #category : #'event handling' }
SpPaginatorSelectionMorph >> handlesMouseOver: anEvent [

	^ true
	
]

{ #category : #initialization }
SpPaginatorSelectionMorph >> initialize [

	super initialize.
	self 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #rigid;
		vResizing: #spaceFill.
	self addResizingEdges
]

{ #category : #accessing }
SpPaginatorSelectionMorph >> minimumWidth [

	^ minimumWidth
]

{ #category : #'event handling' }
SpPaginatorSelectionMorph >> mouseDown: anEvent [ 

	super mouseDown: anEvent.
	lastX := self left.
	lastMouseX := anEvent cursorPoint x
]

{ #category : #'event handling' }
SpPaginatorSelectionMorph >> mouseLeave: anEvent [

	super mouseLeave: anEvent.
	lastMouseX ifNil: [ ^ self ].
	lastMouseX := nil.
	self resetPosition
]

{ #category : #'event handling' }
SpPaginatorSelectionMorph >> mouseMove: anEvent [ 
	| x posX oldPosX |

	super mouseMove: anEvent.

	anEvent redButtonPressed ifFalse: [ ^ self ].
	lastMouseX ifNil: [ ^ self ].

	x := anEvent cursorPoint x.	
	posX := ((self left + (x - lastMouseX)) max: self owner left) min: (self owner right - self width).
	oldPosX := self left.
	self left: posX.
	lastMouseX := x.
	draggingAction ifNotNil: [ 
		draggingAction
			cull: posX - oldPosX
			cull: posX
			cull: oldPosX ]
]

{ #category : #'event handling' }
SpPaginatorSelectionMorph >> mouseUp: anEvent [ 

	super mouseMove: anEvent.
	lastMouseX := nil.
	self announceDragFinished
	
]

{ #category : #private }
SpPaginatorSelectionMorph >> resetPosition [

	lastX ifNil: [ ^ self ].
	self left: lastX.
	lastX := nil
]

{ #category : #accessing }
SpPaginatorSelectionMorph >> updateSizePages: pages pageWidth: pageWidth [ 
	
	(minimumWidth isNil or: [ pageWidth < minimumWidth ])
		ifTrue: [ minimumWidth := pageWidth ].
	self width: pages * pageWidth
]

{ #category : #events }
SpPaginatorSelectionMorph >> whenDragFinishedDo: aBlock [

	dragFinishedAction := aBlock
]

{ #category : #events }
SpPaginatorSelectionMorph >> whenDraggingDo: aBlock [

	draggingAction := aBlock
]
