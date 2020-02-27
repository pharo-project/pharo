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
	#name : #SpMorphicGridLayout,
	#superclass : #LayoutPolicy,
	#instVars : [
		'calculating',
		'layout'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #layout }
SpMorphicGridLayout >> basicLayout: aMorph in: aRect [
	| computation morphs innerBounds origin |
	
	morphs := aMorph submorphs.
	innerBounds := aRect insetBy: self padding.
	computation := (SpMorphicGridLayoutComputation layout: self)
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
SpMorphicGridLayout >> calculateExtentFor: aMorph constraint: constraint computation: computation [
	| cellExtent height width |
	
	cellExtent := computation cellExtentAt: constraint position.
	width := self calculateWidthFor: aMorph base: cellExtent x.
	height := self calculateHeightFor: aMorph base: cellExtent y.
		
	^ width @ height
]

{ #category : #private }
SpMorphicGridLayout >> calculateHeightFor: aMorph base: aNumber [

	"self isRowHomogeneous ifTrue: [ ^ aNumber ].
	aMorph vResizing = #spaceFill ifTrue: [ ^ aNumber ].
	^ aMorph height"
	^ aNumber
]

{ #category : #private }
SpMorphicGridLayout >> calculateWidthFor: aMorph base: aNumber [

	"self isColumnHomogeneous ifTrue: [ ^ aNumber ].
	aMorph hResizing = #spaceFill ifTrue: [ ^ aNumber ].
	^ aMorph width"
	^ aNumber
]

{ #category : #accessing }
SpMorphicGridLayout >> columnConstraintsAt: aNumber [
 
	^ layout columnConstraintsAt: aNumber
]

{ #category : #accessing }
SpMorphicGridLayout >> columnSpacing [

	^ self layout columnSpacing
]

{ #category : #initialization }
SpMorphicGridLayout >> initialize [

	super initialize.
	calculating := false
]

{ #category : #testing }
SpMorphicGridLayout >> isColumnHomogeneous [

	^ self layout isColumnHomogeneous
]

{ #category : #testing }
SpMorphicGridLayout >> isRowHomogeneous [

	^ self layout isRowHomogeneous
]

{ #category : #accessing }
SpMorphicGridLayout >> layout [
	^ layout
]

{ #category : #accessing }
SpMorphicGridLayout >> layout: aGridLayout [

	layout := aGridLayout
]

{ #category : #layout }
SpMorphicGridLayout >> layout: aMorph in: aRect [
	
	calculating ifTrue: [ ^ self ].
	[
		calculating := true. 
		self basicLayout: aMorph in: aRect ]
	ensure: [
		calculating := false ].
]

{ #category : #private }
SpMorphicGridLayout >> layoutMorph: aMorph staringAt: originPoint withComputation: computation [
	| constraints pos extent |
	
	constraints := aMorph valueOfProperty: #gridConstraints.
	pos := originPoint + (computation positionAt: constraints position).

	aMorph vResizing: (self resizingSyle: self isRowHomogeneous).
	aMorph hResizing: (self resizingSyle: self isColumnHomogeneous).
	
	(layout rowConstraintsAt: constraints position y) ifNotNil: [ :c |
		c expand ifNotNil: [ :expand | aMorph vResizing: (self resizingSyle: expand) ] ].
		
	(layout columnConstraintsAt: constraints position x) ifNotNil: [ :c |
		c expand ifNotNil: [ :expand | aMorph hResizing: (self resizingSyle: expand) ] ].

	"if widget is resizable, it will take all the available space. 
	 if widget is not resizable, we need to take into account its alignment constraint, since it 
	 can be placed differently."
	extent := (aMorph hResizing = #spaceFill and: [ aMorph vResizing = #spaceFill ])
		ifTrue: [ computation cellExtentAt: constraints position ]
		ifFalse: [ self calculateExtentFor: aMorph constraint: constraints computation: computation ].
	
	aMorph bounds: (pos extent: extent)
]

{ #category : #accessing }
SpMorphicGridLayout >> padding [

	^ self layout borderWidth
]

{ #category : #private }
SpMorphicGridLayout >> resizingSyle: expand [
	
	^ expand 
		ifTrue: [ #spaceFill ]
		ifFalse: [ #rigid ]
]

{ #category : #accessing }
SpMorphicGridLayout >> rowSpacing [

	^ self layout rowSpacing
]
