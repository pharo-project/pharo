"
I'm a property to define the initial extent of a morph and whether this morph can be resized (horizontally and/or vertically)
"
Class {
	#name : #SpMorphicStyleSheetExtent,
	#superclass : #SpMorphicStyleSheetProperty,
	#instVars : [
		'extent',
		'hResizing',
		'vResizing'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #accessing }
SpMorphicStyleSheetExtent class >> default [

	^ self 
		newExtent: 0@0 
		hResizing: true 
		vResizing: true
]

{ #category : #'instance creation' }
SpMorphicStyleSheetExtent class >> newExtent: aPoint [

	^ self 
		newExtent: aPoint 
		hResizing: false 
		vResizing: false


]

{ #category : #'instance creation' }
SpMorphicStyleSheetExtent class >> newExtent: aPoint hResizing: hBoolean [

	^ self 
		newExtent: aPoint 
		hResizing: hBoolean 
		vResizing: false
]

{ #category : #'instance creation' }
SpMorphicStyleSheetExtent class >> newExtent: aPoint hResizing: hBoolean vResizing: vBoolean [

	^ self basicNew 
		initializeExtent: aPoint 
			hResizing: hBoolean 
			vResizing: vBoolean;
		yourself
]

{ #category : #'instance creation' }
SpMorphicStyleSheetExtent class >> newExtent: aPoint vResizing: vBoolean [

	^ self 
		newExtent: aPoint 
		hResizing: false
		vResizing: vBoolean
]

{ #category : #operations }
SpMorphicStyleSheetExtent >> applyExtentTo: aMorph [
	
	self extent ifNil: [ ^ self ].
	aMorph extent: self extent
]

{ #category : #operations }
SpMorphicStyleSheetExtent >> applyHResizingTo: aMorph [
	
	aMorph hResizing: (self hResizing
		ifTrue: [ #spaceFill ]
		ifFalse: [ #rigid ])
]

{ #category : #operations }
SpMorphicStyleSheetExtent >> applyTo: aMorph [

	self applyExtentTo: aMorph.
	self applyHResizingTo: aMorph.
	self applyVResizingTo: aMorph
]

{ #category : #operations }
SpMorphicStyleSheetExtent >> applyVResizingTo: aMorph [
	
	aMorph vResizing: (self vResizing
		ifTrue: [ #spaceFill ]
		ifFalse: [ #rigid ])
]

{ #category : #style }
SpMorphicStyleSheetExtent >> extent [
	^ extent
]

{ #category : #style }
SpMorphicStyleSheetExtent >> hResizing [
	^ hResizing
]

{ #category : #style }
SpMorphicStyleSheetExtent >> height [
	
	^ self extent y
]

{ #category : #initialization }
SpMorphicStyleSheetExtent >> initializeExtent: aPoint hResizing: hBoolean vResizing: vBoolean [

	self initialize.
	extent := aPoint.
	hResizing := hBoolean.
	vResizing := vBoolean
]

{ #category : #accessing }
SpMorphicStyleSheetExtent >> name [

	^ #extent
]

{ #category : #style }
SpMorphicStyleSheetExtent >> vResizing [
	^ vResizing
]

{ #category : #style }
SpMorphicStyleSheetExtent >> width [
	
	^ self extent x
]
