"
A general user input event like for keyboard or mouse
"
Class {
	#name : 'UserInputEvent',
	#superclass : 'MorphicEvent',
	#instVars : [
		'type',
		'buttons',
		'position',
		'handler',
		'wasHandled'
	],
	#classVars : [
		'ALT_KEY_MASK',
		'CMD_KEY_MASK',
		'CTRL_KEY_MASK',
		'SHIFT_KEY_MASK'
	],
	#category : 'Morphic-Core-Events',
	#package : 'Morphic-Core',
	#tag : 'Events'
}

{ #category : 'initialization' }
UserInputEvent class >> altKeyMask [

	^ ALT_KEY_MASK
]

{ #category : 'initialization' }
UserInputEvent class >> cmdKeyMask [

	^ 	CMD_KEY_MASK
]

{ #category : 'initialization' }
UserInputEvent class >> ctrlKeyMask [

	^ CTRL_KEY_MASK
]

{ #category : 'class initialization' }
UserInputEvent class >> initialize [

	SHIFT_KEY_MASK := 8.
	CMD_KEY_MASK := 2r01000000.
	CTRL_KEY_MASK := 2r00010000.
	ALT_KEY_MASK := 2r00100000
]

{ #category : 'initialization' }
UserInputEvent class >> shiftKeyMask [

	^ SHIFT_KEY_MASK
]

{ #category : 'modifier state' }
UserInputEvent >> altKeyPressed [
	"Answer true if the alt/option key on the keyboard was being held down when this event occurred."

	"00100000 is the bit that marks that option/alt has been pressed"
	^self optionKeyPressed
]

{ #category : 'modifier state' }
UserInputEvent >> anyModifierKeyPressed [
	"ignore, however, the shift keys 'cause that's not REALLY a command key "

	^ self buttons anyMask: CMD_KEY_MASK | ALT_KEY_MASK | CTRL_KEY_MASK
]

{ #category : 'printing' }
UserInputEvent >> buttonString [
	"Return a string identifying the currently pressed buttons"
	| string |
	string := ''.
	self redButtonPressed ifTrue:[string := string,'red '].
	self yellowButtonPressed ifTrue:[string := string,'yellow '].
	self blueButtonPressed ifTrue:[string := string,'blue '].
	^string
]

{ #category : 'accessing' }
UserInputEvent >> buttons [
	"Return the a word encoding the mouse and modifier buttons for this event."

	^ buttons
]

{ #category : 'accessing' }
UserInputEvent >> buttons: anInteger [ 
	buttons := anInteger
]

{ #category : 'modifier state' }
UserInputEvent >> commandKeyPressed [
	"Answer true if the command key on the keyboard was being held down when this event occurred."

	"The mask for command bit pressed"
	^ buttons anyMask: CMD_KEY_MASK
]

{ #category : 'modifier state' }
UserInputEvent >> controlKeyPressed [
	"Answer true if the control key on the keyboard was being held down when this event occurred."

	"The mask for control bit pressed"
	^ buttons anyMask: CTRL_KEY_MASK
]

{ #category : 'initialize' }
UserInputEvent >> copyHandlerState: anEvent [
	"Copy the handler state from anEvent. Used for quickly transferring handler information between transformed events."
	handler := anEvent handler.
	wasHandled := anEvent wasHandled
]

{ #category : 'accessing' }
UserInputEvent >> handler [
	^handler
]

{ #category : 'accessing' }
UserInputEvent >> handler: anObject [
	handler := anObject
]

{ #category : 'modifier state' }
UserInputEvent >> metaKeyPressed [

	^ Smalltalk platform isMacOS
		  ifTrue: [ self commandKeyPressed ]
		  ifFalse: [ self controlKeyPressed ]
]

{ #category : 'printing' }
UserInputEvent >> modifierString [
	"Return a string identifying the currently pressed modifiers"
	| string |
	string := ''.
	self commandKeyPressed ifTrue:[string := string,'CMD '].
	self shiftPressed ifTrue:[string := string,'SHIFT '].
	self controlKeyPressed ifTrue:[string := string,'CTRL '].
	^string
]

{ #category : 'modifier state' }
UserInputEvent >> optionKeyPressed [
	"Answer true if the alt/option key on the keyboard was being held down when this event occurred."

	"00100000 is the bit that marks that option/alt has been pressed"
	^ buttons anyMask: ALT_KEY_MASK
]

{ #category : 'accessing' }
UserInputEvent >> position [
	^position
]

{ #category : 'initialize' }
UserInputEvent >> resetHandlerFields [
	"Reset anything that is used to cross-communicate between two eventual handlers during event dispatch"
	handler := nil.
	wasHandled := false
]

{ #category : 'private' }
UserInputEvent >> setPosition: aPoint [
	position := aPoint
]

{ #category : 'modifier state' }
UserInputEvent >> shiftPressed [
	"Answer true if the shift key on the keyboard was being held down when this event occurred."

	^ buttons anyMask: SHIFT_KEY_MASK
]

{ #category : 'transforming' }
UserInputEvent >> transformBy: aMorphicTransform [
	"Transform the receiver into a local coordinate system."
	position :=  aMorphicTransform globalPointToLocal: position
]

{ #category : 'transforming' }
UserInputEvent >> transformedBy: aMorphicTransform [
	"Return the receiver transformed by the given transform into a local coordinate system."
	^self shallowCopy transformBy: aMorphicTransform
]

{ #category : 'transforming' }
UserInputEvent >> translateBy: delta [
	"add delta to cursorPoint, and return the new event"
	position := position + delta
]

{ #category : 'transforming' }
UserInputEvent >> translatedBy: delta [
	"add delta to cursorPoint, and return the new event"
	^self shallowCopy translateBy: delta
]

{ #category : 'accessing' }
UserInputEvent >> type [
	"Return a symbol indicating the type this event."

	^ type
]

{ #category : 'accessing' }
UserInputEvent >> wasHandled [
	^wasHandled
]

{ #category : 'accessing' }
UserInputEvent >> wasHandled: aBool [
	wasHandled := aBool
]
