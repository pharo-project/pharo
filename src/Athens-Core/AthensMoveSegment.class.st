"
Move segment always indicates starting of a new contour.
If contour ends with close command, then my insntance will have 'closed' flag set to true.

See AthensPathBuilder , which is responsible for building paths.
"
Class {
	#name : #AthensMoveSegment,
	#superclass : #AthensLineSegment,
	#instVars : [
		'closed'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #visitor }
AthensMoveSegment >> accept: aVisitor [
	^ aVisitor moveSegment: self
]

{ #category : #accessing }
AthensMoveSegment >> closed: aBoolean [
	closed := aBoolean
]

{ #category : #testing }
AthensMoveSegment >> isClosed [
	^ closed == true
]

{ #category : #testing }
AthensMoveSegment >> isMove [
	^ true
]

{ #category : #accessing }
AthensMoveSegment >> reopen: aBoolean [
	closed := aBoolean
]

{ #category : #visitor }
AthensMoveSegment >> sendCommandTo: aBuilder [
	^ aBuilder moveTo: point
]

{ #category : #visitor }
AthensMoveSegment >> visitWith: visitor [

	visitor visitMoveSegment: self
]
