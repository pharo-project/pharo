Class {
	#name : #RubSurfaceSelectionShape,
	#superclass : #Object,
	#instVars : [
		'color',
		'borderColor',
		'borderWidth',
		'firstIndex',
		'lastIndex',
		'vertices'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #'instance creation' }
RubSurfaceSelectionShape class >> vertices: aCollectionOfPoints firstIndex: firstCharBlock lastIndex: lastCharBlock color: aColor borderWidth: aBorderWidth borderColor: aBorderColor [
	^ self new
		vertices: aCollectionOfPoints
		firstIndex: firstCharBlock
		lastIndex: lastCharBlock
		color: aColor
		borderWidth: aBorderWidth
		borderColor: aBorderColor

]

{ #category : #accessing }
RubSurfaceSelectionShape >> backgroundColor [
	^ color
]

{ #category : #accessing }
RubSurfaceSelectionShape >> borderColor [
	^ borderColor
]

{ #category : #accessing }
RubSurfaceSelectionShape >> borderWidth [
	^ borderWidth
]

{ #category : #drawing }
RubSurfaceSelectionShape >> bounds [
	^ self polygon bounds
]

{ #category : #drawing }
RubSurfaceSelectionShape >> drawOn: aCanvas [ 
	aCanvas drawPolygon: self vertices color: self backgroundColor borderWidth: borderWidth borderColor:  borderColor
]

{ #category : #drawing }
RubSurfaceSelectionShape >> drawOnAthensCanvas: aCanvas [
	| path |
	path := aCanvas
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: self vertices first.
			self vertices allButFirstDo: [ :pt | builder lineTo: pt ].
			builder close ].
	aCanvas setShape: path.
	aCanvas setPaint: self backgroundColor.
	aCanvas draw.
	borderWidth > 0
		ifTrue: [ 
			(aCanvas setStrokePaint: borderColor)
				capSquare;
				width: borderWidth.
			aCanvas draw ]
]

{ #category : #accessing }
RubSurfaceSelectionShape >> firstIndex [
	^ firstIndex 
]

{ #category : #accessing }
RubSurfaceSelectionShape >> lastIndex [
	^ lastIndex
]

{ #category : #drawing }
RubSurfaceSelectionShape >> polygon [
	^ Polygon vertices: self vertices
]

{ #category : #drawing }
RubSurfaceSelectionShape >> vertices [
	^ vertices
]

{ #category : #initialization }
RubSurfaceSelectionShape >> vertices: aCollectionOfPoints firstIndex: firstCharIndex lastIndex: lastCharIndex color: aColor borderWidth: aBorderWidth borderColor: aBorderColor [
	firstIndex := firstCharIndex.
	lastIndex := lastCharIndex.
	vertices := aCollectionOfPoints.
	color := aColor.
	borderWidth := aBorderWidth.
	borderColor := aBorderColor
]
