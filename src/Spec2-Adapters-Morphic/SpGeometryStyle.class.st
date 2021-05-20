"
# Geometry properties (`SpGeometryStyle`)

Geometry properties controls how the component (morph) will be arranged within its layout.

- hResizing: the component can be resized horizontally? (boolean, default depends on how the morph behaves outside spec)
- vResizing: the component can be resized vertically? (boolean, default depends on how the morph behaves outside spec)
- width: fixed width of the component.
- height: fixed height of the component.
- minWidth: minimum width of the component (to use when `hResizing=true`) 
- minHeight: minimum height of the component (to use when `vResizing=true`) 
- maxWidth: maximum width of the component (to use when `hResizing=true`) 
- maxHeight: maximum height of the component (to use when `vResizing=true`) 

## Usage
The identifier of geometry in the stylesheet is `Geometry`.

```
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
```
"
Class {
	#name : #SpGeometryStyle,
	#superclass : #SpPropertyStyle,
	#instVars : [
		'width',
		'height',
		'minWidth',
		'minHeight',
		'hResizing',
		'vResizing'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpGeometryStyle class >> stonName [
	
	^ 'Geometry'
]

{ #category : #operations }
SpGeometryStyle >> applyTo: aMorph [

	(self widthVariable preferredValueWith: aMorph width) 
		ifNotNil: [ :aNumber | aMorph width: aNumber ].
	(self heightVariable preferredValueWith: aMorph height) 
		ifNotNil: [ :aNumber | aMorph height: aNumber ].
	self minWidthVariable
		ifNotNil: [ :aVariable | aMorph minWidth: aVariable value ].
	self minHeightVariable
		ifNotNil: [ :aVariable | aMorph minHeight: aVariable value ].
	self hResizingVariable 
		ifNotNil: [ :aVariable | aMorph hResizing: (self resizingStringFor: aVariable value) ].
	self vResizingVariable 
		ifNotNil: [ :aVariable | aMorph vResizing: (self resizingStringFor: aVariable value) ]
]

{ #category : #private }
SpGeometryStyle >> geometryValueBetween: baseNumber and: otherNumber [

	^ baseNumber = 0 
		ifTrue: [ otherNumber ]
		ifFalse: [ baseNumber ]
]

{ #category : #accessing }
SpGeometryStyle >> hResizing [

	^ self hResizingVariable value
]

{ #category : #accessing }
SpGeometryStyle >> hResizing: aBoolean [

	hResizing := aBoolean asStyleVariable
]

{ #category : #'accessing variables' }
SpGeometryStyle >> hResizingVariable [

	^ self toVariable: hResizing
]

{ #category : #accessing }
SpGeometryStyle >> height [

	^ self heightVariable value
]

{ #category : #accessing }
SpGeometryStyle >> height: aNumber [
	
	height := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpGeometryStyle >> heightVariable [

	^ self toVariable: (height ifNil: [ 0 ])
]

{ #category : #testing }
SpGeometryStyle >> isGeometry [

	^ true
]

{ #category : #accessing }
SpGeometryStyle >> minHeight [

	^ self minHeightVariable value
]

{ #category : #accessing }
SpGeometryStyle >> minHeight: aNumber [
	
	minHeight := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpGeometryStyle >> minHeightVariable [

	^ self toVariable: minHeight
]

{ #category : #accessing }
SpGeometryStyle >> minWidth [

	^ self minWidthVariable value
]

{ #category : #accessing }
SpGeometryStyle >> minWidth: aNumber [

	minWidth := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpGeometryStyle >> minWidthVariable [

	^ self toVariable: minWidth
]

{ #category : #private }
SpGeometryStyle >> resizingStringFor: expand [

	^ expand
		ifTrue: [ #spaceFill ]
		ifFalse: [ #rigid ]
]

{ #category : #accessing }
SpGeometryStyle >> vResizing [

	^ self vResizingVariable value
]

{ #category : #accessing }
SpGeometryStyle >> vResizing: aBoolean [

	vResizing := aBoolean asStyleVariable
]

{ #category : #'accessing variables' }
SpGeometryStyle >> vResizingVariable [

	^ self toVariable: vResizing
]

{ #category : #accessing }
SpGeometryStyle >> width [

	^ self widthVariable value
]

{ #category : #accessing }
SpGeometryStyle >> width: aNumber [

	width := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpGeometryStyle >> widthVariable [

	^ self toVariable: (width ifNil: [ 0 ])
]
