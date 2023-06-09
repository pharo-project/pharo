"
i represent a path shape in balloon backend
"
Class {
	#name : #AthensBalloonPath,
	#superclass : #AthensPath,
	#instVars : [
		'contours'
	],
	#category : #'Athens-Balloon-Path'
}

{ #category : #accessing }
AthensBalloonPath >> contours [

	^ contours
]

{ #category : #converting }
AthensBalloonPath >> contoursForFilling [
	"Answer a collection of contours, ready for filling by balloon engine"

	^ contours collect: [ :contour |
		  contour last = contour first
			  ifTrue: [ contour asArray ]
			  ifFalse: [
				  contour , {
					  contour last.
					  contour last.
					  contour first } ] ]
]

{ #category : #converting }
AthensBalloonPath >> contoursForStroke [
	"Answer a collection of contours, ready for stroking by balloon engine"

	^ contours collect: [ :contour | contour asArray ]
]

{ #category : #converting }
AthensBalloonPath >> convertFromBuilder: aBuilder [

	| segment cContours |
	segment := aBuilder pathStart.

	[ segment notNil ] whileTrue: [
		segment visitWith: self.
		segment := segment next ].

	"Finally make all OrderedColllections ==> Array "
	cContours := Array new: contours size.
	1 to: cContours size do: [ :i | cContours at: i put: (contours at: i) asArray ].
	contours := cContours
]

{ #category : #private }
AthensBalloonPath >> duplicateLastPoint [

	contours last size < 2 ifFalse: [
		"Duplicate last contour point"
		contours last add: contours last last ]
]

{ #category : #accessing }
AthensBalloonPath >> numContours [

	^ contours size
]

{ #category : #drawing }
AthensBalloonPath >> paintFillsUsing: aPaint on: anAthensCanvas [
	"This method is a part of rendering dispatch  Canvas->receiver->paint"

	^ aPaint athensFillPath: self on: anAthensCanvas
]

{ #category : #converting }
AthensBalloonPath >> visitCloseSegment: closeSegment [
	"See if its already closed"

	contours last last = contours last first ifFalse: [
		contours last
			add: contours last last;
			add: closeSegment endPoint;
			add: closeSegment endPoint ]
]

{ #category : #converting }
AthensBalloonPath >> visitLineSegment: line [

	self duplicateLastPoint.
	contours last
		add: line endPoint;
		add: line endPoint
]

{ #category : #converting }
AthensBalloonPath >> visitMoveSegment: seg [

	contours ifNil: [ contours := OrderedCollection new ].

	contours add: OrderedCollection new.
	contours last add: seg endPoint
]

{ #category : #converting }
AthensBalloonPath >> visitQuadSegment: quadSegment [

	self duplicateLastPoint.

	contours last
		add: quadSegment via;
		add: quadSegment to
]
