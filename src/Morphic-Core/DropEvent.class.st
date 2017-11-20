"
I model the event produced when a hand drops a (grabbed) morph into another morph.
I know the position of the drop, the dropped morph and the hand that produced me.

To handle this event a morph should override one of these methods:
#acceptDroppingMorph:event:
#justDroppedInto:event:

Additionaly, a morph can specify if it wants to accept a dropped morph by overriding #wantsDroppedMorph:event:. 
Symmetrically, the morph being dropped can specify if it wants to be dropped in another morph by overriding #wantsToBeDroppedInto:. 

Note that for a successful drop operation both parties need to agree.

See HandMorph>>dropMorph:event: for an example of usage.

"
Class {
	#name : #DropEvent,
	#superclass : #MorphicEvent,
	#instVars : [
		'position',
		'contents',
		'wasHandled'
	],
	#category : #Morphic-Core-Events
}

{ #category : #accessing }
DropEvent >> contents [
	^contents
]

{ #category : #initialize }
DropEvent >> copyHandlerState: anEvent [
	"Copy the handler state from anEvent. Used for quickly transferring handler information between transformed events."
	wasHandled := anEvent wasHandled.
]

{ #category : #accessing }
DropEvent >> cursorPoint [
	"For compatibility with mouse events"
	^position
]

{ #category : #testing }
DropEvent >> isDropEvent [
	^true
]

{ #category : #accessing }
DropEvent >> position [
	^position
]

{ #category : #printing }
DropEvent >> printOn: aStream [

	aStream nextPut: $[.
	aStream nextPutAll: self position printString; space.
	aStream nextPutAll: self type; space.
	aStream	 nextPutAll: self windowIndex printString.
	aStream nextPut: $].
]

{ #category : #initialize }
DropEvent >> resetHandlerFields [
	"Reset anything that is used to cross-communicate between two eventual handlers during event dispatch"
	wasHandled := false.
]

{ #category : #dispatching }
DropEvent >> sentTo: anObject [
	"Dispatch the receiver into anObject"
	^anObject handleDropMorph: self
]

{ #category : #private }
DropEvent >> setPosition: pos contents: aMorph hand: aHand [
	position := pos.
	contents := aMorph.
	source := aHand.
	wasHandled := false.
]

{ #category : #transforming }
DropEvent >> transformBy: aMorphicTransform [
	"Transform the receiver into a local coordinate system."
	position :=  aMorphicTransform globalPointToLocal: position.
]

{ #category : #transforming }
DropEvent >> transformedBy: aMorphicTransform [
	"Return the receiver transformed by the given transform into a local coordinate system."
	^self shallowCopy transformBy: aMorphicTransform
]

{ #category : #accessing }
DropEvent >> type [
	^#dropEvent
]

{ #category : #accessing }
DropEvent >> wasHandled [
	^wasHandled
]

{ #category : #accessing }
DropEvent >> wasHandled: aBool [
	wasHandled := aBool.
]
