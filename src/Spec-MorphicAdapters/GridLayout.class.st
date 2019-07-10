"
I'm a grid layout.
I can arrange submorphs in a grid according to its properties (position and span, see GrisLayoutProperties), and according certain layout properties: 

- columnHomogeneous -> weather a columns will have same size.
- rowHomogeneous -> weather a row will have same size. 
- padding -> the padding to start drawing cells
- colSpacing -> the column space between cells
- rowSpacing -> the row space between cells

WARNING: I'm designed to work for Spec. While any other morph could benefit from this layout, tipic properties of morphic (like vResizing and hResizing) can not behave as expected. 
"
Class {
	#name : #GridLayout,
	#superclass : #LayoutPolicy,
	#instVars : [
		'rowSpacing',
		'columnSpacing',
		'padding',
		'columnHomogeneous',
		'rowHomogeneous',
		'calculating'
	],
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #layout }
GridLayout >> basicLayout: aMorph in: aRect [
	| computation morphs innerBounds origin |
	
	morphs := aMorph submorphs.
	innerBounds := aRect insetBy: self padding.
	computation := (GridLayoutComputation layout: self)
		computeMorphs: morphs 
		bounds: innerBounds.
	
	"Now calculate the bounds for each widget"
	origin := innerBounds origin.
	morphs do: [ :each | 
		self 
			layoutMorph: each 
			staringAt: origin 
			withComputation: computation ].

	"aMorph bounds: ((origin - self padding) corner: (computation extent + self padding))"
]

{ #category : #private }
GridLayout >> calculateExtentFor: aMorph constraint: constraint computation: computation [
	| cellExtent height width |
	
	cellExtent := computation cellExtentAt: constraint position.
	width := self calculateWidthFor: aMorph base: cellExtent x.
	height := self calculateHeightFor: aMorph base: cellExtent y.
		
	^ width @ height
]

{ #category : #private }
GridLayout >> calculateHeightFor: aMorph base: aNumber [

	self isRowHomogeneous ifTrue: [ ^ aNumber ].
	aMorph vResizing = #spaceFill ifTrue: [ ^ aNumber ].
	^ aMorph height
]

{ #category : #private }
GridLayout >> calculateWidthFor: aMorph base: aNumber [

	self isColumnHomogeneous ifTrue: [ ^ aNumber ].
	aMorph hResizing = #spaceFill ifTrue: [ ^ aNumber ].
	^ aMorph width
]

{ #category : #accessing }
GridLayout >> columnHomogeneous: anObject [

	columnHomogeneous := anObject
]

{ #category : #accessing }
GridLayout >> columnSpacing [

	^ columnSpacing
]

{ #category : #accessing }
GridLayout >> columnSpacing: anObject [

	columnSpacing := anObject
]

{ #category : #initialization }
GridLayout >> initialize [

	super initialize.
	calculating := false.

	self padding: 0.
	self columnSpacing: 0.
	self rowSpacing: 0.
	self columnHomogeneous: false.
	self rowHomogeneous: false
]

{ #category : #testing }
GridLayout >> isColumnHomogeneous [

	^ columnHomogeneous
]

{ #category : #testing }
GridLayout >> isRowHomogeneous [

	^ rowHomogeneous
]

{ #category : #layout }
GridLayout >> layout: aMorph in: aRect [
	
	calculating ifTrue: [ ^ self ].
	[
		calculating := true. 
		self basicLayout: aMorph in: aRect ]
	ensure: [
		calculating := false ].
]

{ #category : #private }
GridLayout >> layoutMorph: aMorph staringAt: originPoint withComputation: computation [
	| constraint pos extent |
	
	constraint := aMorph valueOfProperty: #gridConstraints.
	pos := originPoint + (computation positionAt: constraint position).
	"if widget is resizable, it will take all the available space. 
	 if widget is not resizable, we need to take into account its alignment constraint, since it 
	 can be placed differently."
	extent := (aMorph hResizing = #spaceFill and: [ aMorph vResizing = #spaceFill ])
		ifTrue: [ computation cellExtentAt: constraint position ]
		ifFalse: [ self calculateExtentFor: aMorph constraint: constraint computation: computation ].
	
	aMorph bounds: (pos extent: extent)
]

{ #category : #accessing }
GridLayout >> padding [

	^ padding
]

{ #category : #accessing }
GridLayout >> padding: aNumber [

	padding := aNumber
]

{ #category : #accessing }
GridLayout >> rowHomogeneous: aBoolean [

	rowHomogeneous := aBoolean
]

{ #category : #accessing }
GridLayout >> rowSpacing [
	^ rowSpacing
]

{ #category : #accessing }
GridLayout >> rowSpacing: aBoolean [

	rowSpacing := aBoolean
]
