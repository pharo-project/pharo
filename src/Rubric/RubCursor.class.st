"
I represent a cursor for the mouse 
"
Class {
	#name : #RubCursor,
	#superclass : #Morph,
	#instVars : [
		'stayVisible',
		'invisibleCursorColor',
		'visibleCursorColor',
		'period'
	],
	#classVars : [
		'DefaultColor',
		'DefaultWidth',
		'FixedColor',
		'Period'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #accessing }
RubCursor class >> cursorWithFixedColor [
	^ FixedColor ifNil: [ FixedColor := false ]
]

{ #category : #accessing }
RubCursor class >> cursorWithFixedColor: aBoolean [
	FixedColor := aBoolean
]

{ #category : #accessing }
RubCursor class >> defaultColor [
	^ DefaultColor ifNil: [ DefaultColor := Color lightGray ]
]

{ #category : #accessing }
RubCursor class >> defaultColor: aColor [
	DefaultColor := aColor
]

{ #category : #accessing }
RubCursor class >> defaultWidth [
	^ DefaultWidth ifNil: [ DefaultWidth := 2 ]
]

{ #category : #accessing }
RubCursor class >> defaultWidth: anInteger [
	DefaultWidth := anInteger
]

{ #category : #accessing }
RubCursor class >> period [
	^ Period ifNil: [ Period :=  700 ] 
]

{ #category : #accessing }
RubCursor class >> period: anInteger [
	Period :=  anInteger
]

{ #category : #settings }
RubCursor class >> rubricSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #'Rubric cursor')
		parent: #Rubric;
		label: 'Cursor';
		with: [ 
					(aBuilder setting: #cursorWithFixedColor)
						target: self;
						default: false;
						label: 'Fixed color';
						with: [ 
									(aBuilder setting: #defaultColor)
										target: self;
										default: Color lightGray;
										description: 'The cursolr color color';
										label: 'Color'  ].
					(aBuilder range: #defaultWidth)
						target: self;
						default: 2;
						label: 'Width';
						description: 'Specify the cursor width';
						range: (1 to: 4).
					(aBuilder range: #period)
						target: self;
						default: 700;
						label: 'Blinking period';
						description:
								'Specify the cursor blinking period in milliseconds. If it is set to zero then the cursor will not blink';
						range: (0 to: 1000) ]
]

{ #category : #initialization }
RubCursor >> aboutToBeRemoved [
	self beNotVisible.
	self removeAlarm.
	self textArea announcer unsubscribe: self
]

{ #category : #accessing }
RubCursor >> actualCursorColor [
	self textArea readOnly ifTrue: [ ^ Color transparent ].
	^ (self textArea hasFocus and: [ self stayVisible not ])
		ifFalse: [ self invisibleCursorColor ]
		ifTrue: [ self visibleCursorColor ]
]

{ #category : #adding }
RubCursor >> addAlarm [

	self addAlarmAt: self period
]

{ #category : #adding }
RubCursor >> addAlarmAt: millisecs [
	self removeAlarm.
	(self stayVisible not and: [ self textArea hasFocus ])
		ifFalse: [ ^ self ].
	self world
		ifNotNil: [ :wld | 
			wld
				addAlarm: #hideShow
				withArguments: #()
				for: self
				at: Time millisecondClockValue + millisecs ]
]

{ #category : #blinking }
RubCursor >> beNotVisible [
	self color = self invisibleCursorColor
		ifFalse: [ self color: self invisibleCursorColor ]
]

{ #category : #blinking }
RubCursor >> beVisible [
	self color = self invisibleCursorColor
		ifTrue: [ self color: self actualCursorColor ]
]

{ #category : #initialization }
RubCursor >> defaultBounds [
	^ 0 @ 0 corner: 0 @ 0
]

{ #category : #initialization }
RubCursor >> defaultColor [ 
	^ Color transparent
]

{ #category : #initialization }
RubCursor >> defaultVisibleCursorColor [
	^ self theme textColor
]

{ #category : #blinking }
RubCursor >> ensureFor: millisecs [
	self removeAlarm.
	self beVisible.
	self textArea canChangeText
		ifTrue: [ self addAlarmAt: millisecs ]
]

{ #category : #blinking }
RubCursor >> hideShow [
	self textArea canChangeText 
		ifFalse: [ ^ self beNotVisible ].
	self stayVisible 
		ifTrue: [ ^ self beVisible ].
	self isBlinkingCursor
		ifTrue: [ 
			self switchColor.
			self addAlarm ]
		ifFalse: [ self beVisible ]
]

{ #category : #accessing }
RubCursor >> invisibleCursorColor [
	^ invisibleCursorColor ifNil: [ invisibleCursorColor := Color transparent ]
]

{ #category : #blinking }
RubCursor >> isBlinkingCursor [
	^ self period > 0
]

{ #category : #'submorphs-accessing' }
RubCursor >> noteNewOwner: aMorph [ 
	super noteNewOwner: aMorph.
	self textArea announcer when: MorphGotFocus send: #whenGotFocus: to: self.
	self textArea announcer when: MorphLostFocus send: #whenLostFocus: to: self.
	self textArea announcer when: RubExtentChanged send: #whenTextAreaExtentChanged: to: self.
	self textArea announcer when: RubSelectionChanged send: #whenTextAreaSelectionChanged: to: self.
	self textArea announcer when: RubTextStyleChanged send: #whenTextAreaTextStyleChanged: to: self.

]

{ #category : #private }
RubCursor >> outOfWorld: aWorld [

	super outOfWorld: aWorld.
	
	aWorld removeAlarm: #hideShow for: self.
]

{ #category : #accessing }
RubCursor >> period [
	^ period ifNil: [ period := self class period ]
]

{ #category : #blinking }
RubCursor >> removeAlarm [
	self world
		ifNotNil: [ :wld | 
			wld removeAlarm: #hideShow for: self ] 
]

{ #category : #blinking }
RubCursor >> startBlinking [
	self beVisible.
	self ensureFor: 1000
]

{ #category : #accessing }
RubCursor >> stayVisible [
	^ stayVisible ifNil: [ stayVisible := false ]
]

{ #category : #accessing }
RubCursor >> stayVisible: aBoolean [
	stayVisible := aBoolean.
	self color: self visibleCursorColor 
]

{ #category : #blinking }
RubCursor >> switchColor [
	self color = self invisibleCursorColor
		ifTrue: [ self color: self actualCursorColor ]
		ifFalse: [ self beNotVisible ].

]

{ #category : #accessing }
RubCursor >> textArea [
	^ owner
]

{ #category : #structure }
RubCursor >> topRendererOrSelf [
	^owner topRendererOrSelf
]

{ #category : #private }
RubCursor >> unplug [
	self removeAlarm.
	self textArea announcer unsubscribe: self.
	super unplug
]

{ #category : #'private - layout' }
RubCursor >> updateBounds [
	| x line |
	self textArea ifNil: [ ^self ].
	line := self textArea selectionStop textLine.
	x := self textArea selectionStop left.
	self position: x @ line top.
	self extent: self class defaultWidth @ (line height + 1)
]

{ #category : #'event handling' }
RubCursor >> updateVisibility [
	(self textArea isNil or: [ self textArea canChangeText not ])
		ifTrue: [ ^ self beNotVisible ].
	self beVisible.
	self isBlinkingCursor
		ifTrue: [ self startBlinking ]
		ifFalse: [ self beVisible ].
	self changed
]

{ #category : #accessing }
RubCursor >> visibleCursorColor [
	self class cursorWithFixedColor 
		ifTrue: [ ^ self class defaultColor ].
	^ visibleCursorColor ifNil: [ self defaultVisibleCursorColor ] 
]

{ #category : #accessing }
RubCursor >> visibleCursorColor: aColor [
	visibleCursorColor := aColor
]

{ #category : #'event handling' }
RubCursor >> whenGotFocus: anAnnouncement [
	self updateBounds.
	self updateVisibility 
]

{ #category : #'event handling' }
RubCursor >> whenLostFocus: anAnnouncement [
	self stayVisible
		ifFalse: [ 
			self removeAlarm.
			self beNotVisible ]
]

{ #category : #'event handling' }
RubCursor >> whenTextAreaExtentChanged: anAnnouncement [
	self updateBounds
]

{ #category : #'event handling' }
RubCursor >> whenTextAreaSelectionChanged: anAnnouncement [
	self updateBounds.
	self updateVisibility 
]

{ #category : #'event handling' }
RubCursor >> whenTextAreaTextStyleChanged: anAnnouncement [
	self updateBounds.

]
