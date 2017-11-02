"
i am special kind of shape which using only straight line segments, unlike from more generic paths.
"
Class {
	#name : #AthensPolygon,
	#superclass : #Object,
	#instVars : [
		'contours',
		'currentContour'
	],
	#category : 'Athens-Core-Paths'
}

{ #category : #conversion }
AthensPolygon >> finish [
	"Finally convert contours to arrays"
	
	contours := contours collect: [:ea | ea asArray ]
]

{ #category : #conversion }
AthensPolygon >> lineTo: aPoint [ 
	currentContour add: aPoint
]

{ #category : #conversion }
AthensPolygon >> moveTo: aPoint [ 
	" create a new contour "
	
	self newContour.
	currentContour add: aPoint.
]

{ #category : #conversion }
AthensPolygon >> newContour [
	contours ifNil: [ contours := OrderedCollection new ].
	
	currentContour := contours add: OrderedCollection new.
]

{ #category : #drawing }
AthensPolygon >> paintFillsUsing: aPaint on: anAthensCanvas [ 
	
	"This method is a part of rendering dispatch  Canvas->receiver->paint"
	
	^ aPaint fillPolygon: self on: anAthensCanvas
]
