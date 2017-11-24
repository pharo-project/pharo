"
A button event from a mouse
"
Class {
	#name : #MouseButtonEvent,
	#superclass : #MouseEvent,
	#instVars : [
		'whichButton'
	],
	#category : #'Morphic-Core-Events'
}

{ #category : #accessing }
MouseButtonEvent >> blueButtonChanged [
	"Answer true if the blue mouse button has changed. This is the third mouse button or cmd+click on the Mac."

	^ whichButton anyMask: self class blueButton
]

{ #category : #accessing }
MouseButtonEvent >> redButtonChanged [
	"Answer true if the red mouse button has changed. This is the first mouse button."

	^ whichButton anyMask: self class redButton
]

{ #category : #dispatching }
MouseButtonEvent >> sentTo: anObject [
	"Dispatch the receiver into anObject"
	type == #mouseDown ifTrue:[^anObject handleMouseDown: self].
	type == #mouseUp ifTrue:[^anObject handleMouseUp: self].
	^super sentTo: anObject
]

{ #category : #private }
MouseButtonEvent >> setType: evtType position: evtPos which: button buttons: evtButtons hand: evtHand stamp: stamp [
	type := evtType.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := button.
	timeStamp := stamp.
]

{ #category : #printing }
MouseButtonEvent >> storeOn: aStream [
	super storeOn: aStream.
	aStream space.
	whichButton storeOn: aStream.
]

{ #category : #accessing }
MouseButtonEvent >> whichButton [
	^whichButton
]

{ #category : #accessing }
MouseButtonEvent >> yellowButtonChanged [
	"Answer true if the yellow mouse button has changed. This is the second mouse button or option+click on the Mac."

	^ whichButton anyMask: self class yellowButton
]
