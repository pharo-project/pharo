"
Groups a set of sounds to use with a UITheme.
Note that, although instances may have their sounds changed the settings will still indicate the class of theme that is current.
"
Class {
	#name : #SoundTheme,
	#superclass : #Object,
	#instVars : [
		'sounds'
	],
	#classVars : [
		'Current',
		'UseThemeSounds'
	],
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #accessing }
SoundTheme class >> allThemeClasses [
	"Answer the subclasses of the receiver that are considered to be
	concrete (useable as a theme)."

	^self withAllSubclasses reject: [:c | c isAbstract]
]

{ #category : #accessing }
SoundTheme class >> beCurrent [
	"Make a new instance of the receiver be the current theme."

	self isAbstract ifTrue: [^self error: self name, ' is abstract, send #beCurrent to a subclass.'].
	self current: self newDefault
]

{ #category : #accessing }
SoundTheme class >> current [
	"Answer the current ui theme."

	^Current ifNil: [Current := NullSoundTheme newDefault. Current]
]

{ #category : #accessing }
SoundTheme class >> current: aSoundTheme [
	"Set the current sound theme."

	Current := aSoundTheme
]

{ #category : #testing }
SoundTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : #testing }
SoundTheme class >> isCurrent [
	"Answer whether an instance of the receiver is the current theme."

	^self current class == self
]

{ #category : #'instance creation' }
SoundTheme class >> newDefault [
	"Answer a new instance of the sound theme with
	its default sounds."

	^self new
]

{ #category : #defaults }
SoundTheme class >> themeName [
	"Answer the friendly name of the theme."

	^'Beeper sounds'
]

{ #category : #settings }
SoundTheme class >> useThemeSounds [
	 ^ UseThemeSounds ifNil: [UseThemeSounds := true]
]

{ #category : #settings }
SoundTheme class >> useThemeSounds: aBoolean [
	UseThemeSounds := aBoolean
]

{ #category : #sounds }
SoundTheme >> abortSound [
	"Answer the abort sound."

	^self sounds at: #abort ifAbsent: [self defaultSound]
]

{ #category : #sounds }
SoundTheme >> alertSound [
	"Answer the alert sound."

	^self sounds at: #alert ifAbsent: [self defaultSound]
]

{ #category : #initialization }
SoundTheme >> defaultAbortSound [
	"Answer the default abort sound."

	^self defaultSound
]

{ #category : #'initialize-release' }
SoundTheme >> defaultAlertSound [
	"Answer the default alert sound."

	^self defaultSound
]

{ #category : #initialization }
SoundTheme >> defaultDefaultSound [
	"Answer the default default sound!"

	^Beeper default
]

{ #category : #'initialize-release' }
SoundTheme >> defaultDenySound [
	"Answer the default deny sound."

	^self defaultSound
]

{ #category : #initialization }
SoundTheme >> defaultMessageSound [
	"Answer the default message sound."

	^self defaultSound
]

{ #category : #initialization }
SoundTheme >> defaultQuestionSound [
	"Answer the default question sound."

	^self defaultSound
]

{ #category : #sounds }
SoundTheme >> defaultSound [
	"Answer the default sound."

	^self sounds at: #default ifAbsent: [self defaultDefaultSound]
]

{ #category : #initialization }
SoundTheme >> defaultWindowCloseSound [
	"Answer the default window close sound."

	^self defaultSound
]

{ #category : #initialization }
SoundTheme >> defaultWindowMaximizeSound [
	"Answer the default window maximize sound."

	^self defaultSound
]

{ #category : #initialization }
SoundTheme >> defaultWindowMinimizeSound [
	"Answer the default window minimize sound."

	^self defaultSound
]

{ #category : #'initialize-release' }
SoundTheme >> defaultWindowOpenSound [
	"Answer the default window open sound."

	^self defaultSound
]

{ #category : #initialization }
SoundTheme >> defaultWindowRestoreDownSound [
	"Answer the default window restore down sound."

	^self defaultSound
]

{ #category : #initialization }
SoundTheme >> defaultWindowRestoreUpSound [
	"Answer the default window restore up sound."

	^self defaultSound
]

{ #category : #sounds }
SoundTheme >> denySound [
	"Answer the deny sound."

	^self sounds at: #deny ifAbsent: [self defaultSound]
]

{ #category : #initialization }
SoundTheme >> initialize [
	"Initialize the receiver."

	super initialize.
	self initializeSounds
]

{ #category : #initialization }
SoundTheme >> initializeSounds [
	"Initialize the receiver's event sounds."

	self sounds: Dictionary new.
	self sounds
		at: #default put: self defaultDefaultSound;
		at: #abort put: self defaultAbortSound;
		at: #alert put: self defaultAlertSound;
		at: #deny put: self defaultDenySound;
		at: #message put: self defaultMessageSound;
		at: #question put: self defaultQuestionSound;
		at: #windowMinimize put: self defaultWindowMinimizeSound;
		at: #windowMaximize put: self defaultWindowMaximizeSound;
		at: #windowRestoreUp put: self defaultWindowRestoreUpSound;
		at: #windowRestoreDown put: self defaultWindowRestoreDownSound;
		at: #windowClose put: self defaultWindowCloseSound
]

{ #category : #sounds }
SoundTheme >> messageSound [
	"Answer the message sound."

	^self sounds at: #message ifAbsent: [self defaultSound]
]

{ #category : #sounds }
SoundTheme >> questionSound [
	"Answer the question sound."

	^self sounds at: #question ifAbsent: [self defaultSound]
]

{ #category : #accessing }
SoundTheme >> sounds [
	"Answer the value of sounds"

	^ sounds
]

{ #category : #accessing }
SoundTheme >> sounds: anObject [
	"Set the value of sounds"

	sounds := anObject
]

{ #category : #sounds }
SoundTheme >> windowCloseSound [
	"Answer the window close sound."

	^self sounds at: #windowClose ifAbsent: [self defaultSound]
]

{ #category : #sounds }
SoundTheme >> windowMaximizeSound [
	"Answer the window maximize sound."

	^self sounds at: #windowMaximize ifAbsent: [self defaultSound]
]

{ #category : #sounds }
SoundTheme >> windowMinimizeSound [
	"Answer the window minimize sound."

	^self sounds at: #windowMinimize ifAbsent: [self defaultSound]
]

{ #category : #sounds }
SoundTheme >> windowOpenSound [
	"Answer the window open sound."

	^self sounds at: #windowOpen ifAbsent: [self defaultSound]
]

{ #category : #sounds }
SoundTheme >> windowRestoreDownSound [
	"Answer the window restore down sound."

	^self sounds at: #windowRestoreDown ifAbsent: [self defaultSound]
]

{ #category : #sounds }
SoundTheme >> windowRestoreUpSound [
	"Answer the window restore up sound."

	^self sounds at: #windowRestoreUp ifAbsent: [self defaultSound]
]
