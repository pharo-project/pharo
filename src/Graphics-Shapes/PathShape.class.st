"
I represent a model of an open sequence of connected points that can be queried for
enclosing bounds and whether a point lies along any segment.
 
I am typically used for drawing with a Canvas.

ps := PathShape new
        addVertex: 0@0;
        addVertex: 30@30;
        addVertex: 50@10.
        
self assert: (ps containsPoint: 24@24).
self assert: (ps containsPoint: 40@30) not.
Transcript show: ps calculatedBounds asString; cr.
"
Class {
	#name : #PathShape,
	#superclass : #Shape,
	#instVars : [
		'bounds',
		'vertices'
	],
	#category : #'Graphics-Shapes'
}

{ #category : #'instance creation' }
PathShape class >> vertices: aCollection [
	"Answer a new instance of the receiver with the
	given vertices."

	^self new
		vertices: aCollection
]

{ #category : #adding }
PathShape >> addVertex: aPoint [
	"Add a vertex to the path."

	self vertices add: aPoint.
	self basicBounds ifNotNil: [
		self bounds: (self bounds quickMergePoint: aPoint)] 
]

{ #category : #accessing }
PathShape >> basicBounds [
	"Answer the bounds of the receiver without lazy calculation."

	^bounds
]

{ #category : #accessing }
PathShape >> bounds [
	"Answer the bounds of the receiver."

	^bounds ifNil: [bounds := self calculatedBounds]
]

{ #category : #accessing }
PathShape >> bounds: anObject [
	"Set the value of bounds"

	bounds := anObject
]

{ #category : #compute }
PathShape >> calculatedBounds [
	"Answer the bounds of the receiver calculated from the
	receiver's vertices."

	|tl br|
	self vertices ifEmpty: [^nil].
	tl := br := self vertices first.
	self vertices allButFirstDo: [:v |
		tl := tl min: v.
		br := br max: v].
	^tl corner: br + 1
]

{ #category : #testing }
PathShape >> containsPoint: aPoint [
	"Answer whether the receiver contains the given point."

	(self basicContainsPoint: aPoint) ifFalse: [^false].
	self segmentsDo: [:p1 :p2 |
		(aPoint onLineFrom: p1 to: p2 within: 0) ifTrue: [^true]].
	^false
]

{ #category : #initialization }
PathShape >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		vertices: OrderedCollection new
]

{ #category : #enumerating }
PathShape >> segmentsDo: aBlock [
	"Evaluate the two-argument block with each vertex and its successor."

	self vertices size < 2 ifTrue: [^self].
	1 to: self vertices size - 1 do: [:i |
		aBlock
			value: (self vertices at: i)
			value: (self vertices at: i + 1)]
]

{ #category : #accessing }
PathShape >> vertices [
	"Answer the value of vertices"

	^ vertices
]

{ #category : #accessing }
PathShape >> vertices: aCollection [
	"Set the value of vertices."

	vertices := aCollection asOrderedCollection.
	self bounds: nil
]
