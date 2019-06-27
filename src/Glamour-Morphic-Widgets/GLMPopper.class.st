"
This is a morph used for displaying various notifications.

It has a special ability to delete itself either when Esc is pressed, or when the focus is lost.

When opened the user has to spefify a text morph whose position and cursor are user to position this morph.

When triggered from a context menu action the class method installAlarmFor: should be used to open the popper as the text morph gets back the focus after the action was executed (and, hence, the popper is closed).
"
Class {
	#name : #GLMPopper,
	#superclass : #Morph,
	#instVars : [
		'textMorph'
	],
	#category : #'Glamour-Morphic-Widgets-Popper'
}

{ #category : #utilities }
GLMPopper class >> installAlarm: aSelector for: anObject [

	self currentWorld
		addAlarm: aSelector
		withArguments: #()
		for: anObject
		at: Time millisecondClockValue + 50.
]

{ #category : #utilities }
GLMPopper class >> installAlarm: aSelector for: anObject withArguments: anArray [

	self currentWorld
		addAlarm: aSelector
		withArguments: anArray
		for: anObject
		at: Time millisecondClockValue + 50.
]

{ #category : #public }
GLMPopper >> computeOriginFor: aMorph [
	| desiredTop desiredLeft |

	aMorph textArea scrollSelectionIntoView: nil.
	desiredTop := (self originMorphFor: aMorph) positionInWorld y - 6.
						"unclear why 6 is the magic number"
	desiredLeft := (self originMorphFor: aMorph) right + aMorph positionInWorld x + 6.
	^ desiredLeft @ desiredTop
]

{ #category : #initialization }
GLMPopper >> initialize [
	super initialize.
	self changeProportionalLayout.
	self
		onAnnouncement: MorphLostFocus do: [ :ann | ann morph delete ];
		color: Color transparent
]

{ #category : #public }
GLMPopper >> originMorphFor: aMorph [

	(aMorph textArea hasSelection not and: [ 
		aMorph textArea hasHighlightSegment ]) ifTrue: [ 
			^ aMorph textArea getHighlightSegment ].
	^ aMorph textArea cursor
]

{ #category : #private }
GLMPopper >> updateWithString: string from: aMorph [
	textMorph textArea
		updateTextWith: string;
		takeKeyboardFocus;
		on: Character escape do: [ 
			self delete.
			aMorph textArea removeHighlightSegment.
			aMorph takeKeyboardFocus ];
		on: PharoShortcuts current cancelShortcut do: [ 
			self delete.
			aMorph textArea editor cancel.
			aMorph takeKeyboardFocus ];
		on: Character backspace do: [ 
			self delete.
			aMorph textArea removeHighlightSegment.
			aMorph takeKeyboardFocus ];
		onAnnouncement: MorphLostFocus do: [ :ann | 
			self delete.
			aMorph textArea removeHighlightSegment.
			"If this event is triggered then another morph already has the focus.
			Do not attempt to give the focus back to the original morph" ]
]

{ #category : #private }
GLMPopper >> withString: string from: aMorph [
	| top width left stringMorph height desiredOrigin |
	desiredOrigin := self computeOriginFor: aMorph.
	stringMorph := RubEditingArea new 
		font: self theme textFont;
		updateTextWith: string.
	width := stringMorph width + 12 min: self currentWorld width / 3.
	left := desiredOrigin x + width > self currentWorld width
		ifTrue: [ self currentWorld width - width ]
		ifFalse: [ desiredOrigin x ].
	height := (stringMorph
		beWrapped;
		width: width;
		yourself) height + 2 min: self currentWorld height / 3.
	top := desiredOrigin y + height > self currentWorld height
		ifTrue: [ self currentWorld height - height ]
		ifFalse: [ desiredOrigin y ].
	self updateWithString: string from: aMorph.
	self
		top: top;
		left: left;
		width: width;
		height: height
]
