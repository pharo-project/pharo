"
! Geometry properties (==SpStyleGeometry==)

Geometry properties controls how the component (morph) will be arranged within its layout.

- hResizing: the component can be resized horizontally? (boolean, default depends on how the morph behaves outside spec)
- vResizing: the component can be resized vertically? (boolean, default depends on how the morph behaves outside spec)
- width: fixed width of the component.
- height: fixed height of the component.
- minWidth: minimum width of the component (to use when ==hResizing=true==) 
- minHeight: minimum height of the component (to use when ==vResizing=true==) 
- maxWidth: maximum width of the component (to use when ==hResizing=true==) 
- maxHeight: maximum height of the component (to use when ==vResizing=true==) 

!! Usage
The identifier of geometry in the stylesheet is ==Geometry==.

[[[
Geometry { 
	#hResizing: false,
	#vResizing: false,
	#width: 100,
	#height: 25,
	#minWidth: 50,
	#minHeight: 25,
	#maxWidth: 150,
	#maxHeight: 25
}
]]]
"
Class {
	#name : #SpStyleGeometry,
	#superclass : #SpStyleProperty,
	#instVars : [
		'width',
		'height',
		'minWidth',
		'minHeight',
		'maxWidth',
		'maxHeight',
		'hResizing',
		'vResizing'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleGeometry class >> stonName [
	
	^ 'Geometry'
]

{ #category : #operations }
SpStyleGeometry >> applyTo: aMorph [

	aMorph
		width: (self geometryValueBetween: self width and: aMorph width);		
		height: (self geometryValueBetween: self height and: aMorph height).
		
	self hResizing ifNotNil: [ :aBoolean | 
		aMorph hResizing: (self resizingStringFor: aBoolean) ].
	self vResizing ifNotNil: [ :aBoolean |
		aMorph vResizing: (self resizingStringFor: aBoolean) ]
]

{ #category : #private }
SpStyleGeometry >> calculatedExtent [

	self extent ifNotNil: [ :aPoint | ^ aPoint ].
	self minExtent ifNotNil: [ :aPoint | aPoint ].
	
	^ 10@10
]

{ #category : #accessing }
SpStyleGeometry >> extent [

	^ self width @ self height
]

{ #category : #accessing }
SpStyleGeometry >> extent: aPoint [ 

	width := aPoint x.
	height := aPoint y
]

{ #category : #private }
SpStyleGeometry >> geometryValueBetween: baseNumber and: otherNumber [

	^ baseNumber = 0 
		ifTrue: [ otherNumber ]
		ifFalse: [ baseNumber ]
]

{ #category : #accessing }
SpStyleGeometry >> hResizing [

	^ hResizing
]

{ #category : #accessing }
SpStyleGeometry >> hResizing: aBoolean [

	hResizing := aBoolean
]

{ #category : #accessing }
SpStyleGeometry >> height [

	^ height ifNil: [ 0 ]
]

{ #category : #accessing }
SpStyleGeometry >> height: aNumber [
	
	height := aNumber
]

{ #category : #initialization }
SpStyleGeometry >> initialize [

	super initialize.
	self hResizing: nil.
	self vResizing: nil
]

{ #category : #accessing }
SpStyleGeometry >> maxExtent [

	^ self maxWidth @ self maxHeight
]

{ #category : #accessing }
SpStyleGeometry >> maxExtent: aPoint [

	maxWidth := aPoint x.
	maxHeight := aPoint y
]

{ #category : #accessing }
SpStyleGeometry >> maxHeight [

	^ maxHeight ifNil: [ 0 ]
]

{ #category : #accessing }
SpStyleGeometry >> maxHeight: aNumber [
	
	maxHeight := aNumber
]

{ #category : #accessing }
SpStyleGeometry >> maxWidth [

	^ maxWidth ifNil: [ 0 ]
]

{ #category : #accessing }
SpStyleGeometry >> maxWidth: aNumber [

	maxWidth := aNumber
]

{ #category : #accessing }
SpStyleGeometry >> minExtent [

	^ self minWidth @ self minHeight
]

{ #category : #accessing }
SpStyleGeometry >> minExtent: aPoint [

	minWidth := aPoint x.
	minHeight := aPoint y
]

{ #category : #accessing }
SpStyleGeometry >> minHeight [

	^ minHeight ifNil: [ 0 ]
]

{ #category : #accessing }
SpStyleGeometry >> minHeight: aNumber [
	
	minHeight := aNumber
]

{ #category : #accessing }
SpStyleGeometry >> minWidth [

	^ minWidth ifNil: [ 0 ]
]

{ #category : #accessing }
SpStyleGeometry >> minWidth: aNumber [

	minWidth := aNumber
]

{ #category : #private }
SpStyleGeometry >> resizingStringFor: expand [

	^ expand
		ifTrue: [ #spaceFill ]
		ifFalse: [ #rigid ]
]

{ #category : #accessing }
SpStyleGeometry >> vResizing [

	^ vResizing
]

{ #category : #accessing }
SpStyleGeometry >> vResizing: aBoolean [

	vResizing := aBoolean
]

{ #category : #accessing }
SpStyleGeometry >> width [

	^ width ifNil: [ 0 ]
]

{ #category : #accessing }
SpStyleGeometry >> width: aNumber [

	width := aNumber
]
