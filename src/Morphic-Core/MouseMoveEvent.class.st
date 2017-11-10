"
A move event from the mouse
"
Class {
	#name : #MouseMoveEvent,
	#superclass : #MouseEvent,
	#instVars : [
		'startPoint',
		'trail'
	],
	#category : #Morphic-Core-Events
}

{ #category : #comparing }
MouseMoveEvent >> = aMorphicEvent [
	super = aMorphicEvent ifFalse:[^false].
	position = aMorphicEvent position ifFalse: [^ false].
	startPoint = aMorphicEvent startPoint ifFalse: [^ false].
	buttons = aMorphicEvent buttons ifFalse: [^ false].
	^ true

]

{ #category : #accessing }
MouseMoveEvent >> endPoint [
	"Return the point where the movement ended."
	^position
]

{ #category : #comparing }
MouseMoveEvent >> hash [
	^ position hash + startPoint hash + buttons hash
]

{ #category : #testing }
MouseMoveEvent >> isMove [
	^true
]

{ #category : #printing }
MouseMoveEvent >> printOn: aStream [

	aStream nextPut: $[.
	aStream nextPutAll: self startPoint printString; space.
	aStream nextPutAll: self endPoint printString; space.
	aStream nextPutAll: self type; space.
	aStream nextPutAll: self modifierString.
	aStream nextPutAll: self buttonString.
	aStream nextPutAll: timeStamp printString; space.
	aStream	 nextPutAll: self windowIndex printString.
	aStream nextPut: $].
]

{ #category : #dispatching }
MouseMoveEvent >> sentTo: anObject [
	"Dispatch the receiver into anObject"
	type == #mouseMove ifTrue:[^anObject handleMouseMove: self].
	^super sentTo: anObject.

]

{ #category : #private }
MouseMoveEvent >> setType: evtType startPoint: evtStart endPoint: evtEnd trail: evtTrail buttons: evtButtons hand: evtHand stamp: stamp [
	type := evtType.
	startPoint := evtStart.
	position := evtEnd.
	trail := evtTrail.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.
]

{ #category : #accessing }
MouseMoveEvent >> startPoint [
	"Return the point where the movement started."
	^startPoint
]

{ #category : #printing }
MouseMoveEvent >> storeOn: aStream [
	super storeOn: aStream.
	aStream space.
	self startPoint x storeOn: aStream.
	aStream space.
	self startPoint y storeOn: aStream.
	aStream space.
	"trail storeOn: aStream."
]

{ #category : #accessing }
MouseMoveEvent >> trail [
	"Return any immediate points that have been assembled along the move"
	^trail ifNil:[#()]
]

{ #category : #transforming }
MouseMoveEvent >> transformBy: aMorphicTransform [
	"Transform the receiver into a local coordinate system."
	position :=  aMorphicTransform globalPointToLocal: position.
	startPoint :=  aMorphicTransform globalPointToLocal: startPoint.
]

{ #category : #transforming }
MouseMoveEvent >> translateBy: delta [
	"add delta to cursorPoint, and return the new event"
	position := position + delta.
	startPoint := startPoint + delta.
]
