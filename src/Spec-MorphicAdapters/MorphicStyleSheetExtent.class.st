"
I'm a property to define the initial extent of a morph and weather this morph can be resized (horizontally and/or vertically)
"
Class {
	#name : #MorphicStyleSheetExtent,
	#superclass : #MorphicStyleSheetProperty,
	#instVars : [
		'extent',
		'hResizing',
		'vResizing'
	],
	#category : #'Spec-MorphicAdapters-StyleSheet'
}

{ #category : #'instance creation' }
MorphicStyleSheetExtent class >> newExtent: aPoint [

	^ self 
		newExtent: aPoint 
		hResizing: false 
		vResizing: false


]

{ #category : #'instance creation' }
MorphicStyleSheetExtent class >> newExtent: aPoint hResizing: hBoolean [

	^ self 
		newExtent: aPoint 
		hResizing: hBoolean 
		vResizing: false
]

{ #category : #'instance creation' }
MorphicStyleSheetExtent class >> newExtent: aPoint hResizing: hBoolean vResizing: vBoolean [

	^ self basicNew 
		initializeExtent: aPoint 
			hResizing: hBoolean 
			vResizing: vBoolean;
		yourself
]

{ #category : #'instance creation' }
MorphicStyleSheetExtent class >> newExtent: aPoint vResizing: vBoolean [

	^ self 
		newExtent: aPoint 
		hResizing: false
		vResizing: vBoolean
]

{ #category : #operations }
MorphicStyleSheetExtent >> applyExtentTo: aMorph [
	
	self extent ifNil: [ ^ self ].
	aMorph extent: self extent
]

{ #category : #operations }
MorphicStyleSheetExtent >> applyHResizingTo: aMorph [
	
	aMorph hResizing: (self hResizing
		ifTrue: [ #spaceFill ]
		ifFalse: [ #rigid ])
]

{ #category : #operations }
MorphicStyleSheetExtent >> applyTo: aMorph [

	self applyExtentTo: aMorph.
	self applyHResizingTo: aMorph.
	self applyVResizingTo: aMorph
]

{ #category : #operations }
MorphicStyleSheetExtent >> applyVResizingTo: aMorph [
	
	aMorph vResizing: (self vResizing
		ifTrue: [ #spaceFill ]
		ifFalse: [ #rigid ])
]

{ #category : #style }
MorphicStyleSheetExtent >> extent [
	^ extent
]

{ #category : #style }
MorphicStyleSheetExtent >> hResizing [
	^ hResizing
]

{ #category : #style }
MorphicStyleSheetExtent >> height [
	
	^ self extent y
]

{ #category : #initialization }
MorphicStyleSheetExtent >> initializeExtent: aPoint hResizing: hBoolean vResizing: vBoolean [

	self initialize.
	extent := aPoint.
	hResizing := hBoolean.
	vResizing := vBoolean
]

{ #category : #accessing }
MorphicStyleSheetExtent >> name [

	^ #extent
]

{ #category : #style }
MorphicStyleSheetExtent >> vResizing [
	^ vResizing
]

{ #category : #style }
MorphicStyleSheetExtent >> width [
	
	^ self extent x
]
