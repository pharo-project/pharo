"
This class represents the base for all events.

Instance variables:
	stamp	<Integer>	The millisecond clock time stamp (based on Time millisecondClock)
	source	<Hand | nil>	If non-nil the hand that generated the event.
"
Class {
	#name : #MorphicEvent,
	#superclass : #Object,
	#instVars : [
		'timeStamp',
		'source',
		'windowIndex'
	],
	#category : #'Morphic-Core-Events'
}

{ #category : #comparing }
MorphicEvent >> = anEvent [
	anEvent isMorphicEvent ifFalse:[^false].
	^self type = anEvent type
]

{ #category : #initialize }
MorphicEvent >> copyHandlerState: anEvent [
	"Copy the handler state from anEvent. Used for quickly transferring handler information between transformed events."

]

{ #category : #accessing }
MorphicEvent >> cursorPoint [
	"Backward compatibility. Use #position instead"
	^ self position
]

{ #category : #accessing }
MorphicEvent >> hand [
	"Return the source that generated the event"
	^source
]

{ #category : #comparing }
MorphicEvent >> hash [
	^self type hash
]

{ #category : #testing }
MorphicEvent >> isDraggingEvent [
	^false
]

{ #category : #testing }
MorphicEvent >> isDropEvent [
	^false
]

{ #category : #testing }
MorphicEvent >> isKeyboard [
	^false
]

{ #category : #testing }
MorphicEvent >> isKeystroke [
	^false
]

{ #category : #testing }
MorphicEvent >> isMorphicEvent [
	^true
]

{ #category : #testing }
MorphicEvent >> isMouse [
	^false
]

{ #category : #testing }
MorphicEvent >> isMouseOver [
	^self type == #mouseOver
]

{ #category : #testing }
MorphicEvent >> isMove [
	^false
]

{ #category : #testing }
MorphicEvent >> isWindowEvent [
	^false
]

{ #category : #accessing }
MorphicEvent >> position [
	"Since cursorPoint is defined and refers to position it should be defined
	here as well"
	^ self subclassResponsibility
]

{ #category : #initialize }
MorphicEvent >> resetHandlerFields [
	"Reset anything that is used to cross-communicate between two eventual handlers during event dispatch"
]

{ #category : #dispatching }
MorphicEvent >> sentTo: anObject [
	"Dispatch the receiver into anObject"
	^anObject handleUnknownEvent: self
]

{ #category : #private }
MorphicEvent >> setHand: aHand [
	source := aHand
]

{ #category : #private }
MorphicEvent >> setTimeStamp: stamp [
	timeStamp := stamp.
]

{ #category : #accessing }
MorphicEvent >> timeStamp [
	"Return the millisecond clock value at which the event was generated"
	^timeStamp ifNil:[timeStamp := Time millisecondClockValue]
]

{ #category : #transforming }
MorphicEvent >> transformedBy: aMorphicTransform [
	"Return the receiver transformed by the given transform into a local coordinate system."

]

{ #category : #accessing }
MorphicEvent >> type [
	"Return a symbol indicating the type this event."
	^self subclassResponsibility
]

{ #category : #accessing }
MorphicEvent >> wasHandled [
	"Return true if this event was handled. May be ignored for some types of events."
	^false
]

{ #category : #accessing }
MorphicEvent >> wasHandled: aBool [
	"Determine if this event was handled. May be ignored for some types of events."
]

{ #category : #accessing }
MorphicEvent >> windowIndex [
	^windowIndex
]

{ #category : #accessing }
MorphicEvent >> windowIndex: aValue [
	windowIndex := aValue
]
