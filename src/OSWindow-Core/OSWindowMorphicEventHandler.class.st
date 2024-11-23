"
I can be used to convert OSWindow-level events to Morphic events.

So, that installing my instance as event handler for specific window would allow running Morphic World in it.

Later, the Morphic can be integrated with OSWindow API to avoid unnecessary conversion and thus eliminating the need in having this class.
"
Class {
	#name : 'OSWindowMorphicEventHandler',
	#superclass : 'OSWindowEventVisitor',
	#instVars : [
		'morphicWorld'
	],
	#classVars : [
		'SimulateMiddleButton',
		'SymbolCharacterMapping'
	],
	#pools : [
		'OSKeySymbols'
	],
	#category : 'OSWindow-Core-Morphic',
	#package : 'OSWindow-Core',
	#tag : 'Morphic'
}

{ #category : 'instance creation' }
OSWindowMorphicEventHandler class >> for: morphicWorld [
	^ self new morphicWorld: morphicWorld; yourself
]

{ #category : 'class initialization' }
OSWindowMorphicEventHandler class >> initialize [
	"
	self initialize
	"

	SymbolCharacterMapping := Dictionary new.
	{
		OSK_RETURN.
		Character cr.
		OSK_BACKSPACE.
		Character backspace.
		OSK_TAB.
		Character tab.
		OSK_HOME.
		Character home.
		OSK_LEFT.
		Character arrowLeft.
		OSK_UP.
		Character arrowUp.
		OSK_RIGHT.
		Character arrowRight.
		OSK_DOWN.
		Character arrowDown.
		OSK_END.
		Character end.
		OSK_INSERT.
		Character insert.
		OSK_PAGEUP.
		Character pageUp.
		OSK_PAGEDOWN.
		Character pageDown.
		OSK_DELETE.
		Character delete.

		OSK_KP_0.
		$0.
		OSK_KP_1.
		$1.
		OSK_KP_2.
		$2.
		OSK_KP_3.
		$3.
		OSK_KP_4.
		$4.
		OSK_KP_5.
		$5.
		OSK_KP_6.
		$6.
		OSK_KP_7.
		$7.
		OSK_KP_8.
		$8.
		OSK_KP_9.
		$9.
		OSK_KP_DIVIDE.
		$/.
		OSK_KP_MULTIPLY.
		$*.
		OSK_KP_PLUS.
		$+.
		OSK_KP_MINUS.
		$-.
		OSK_KP_ENTER.
		Character cr.
		OSK_KP_PERIOD.
		$. } pairsDo: [ :key :val |
		SymbolCharacterMapping at: key put: val charCode ]
]

{ #category : 'event testing' }
OSWindowMorphicEventHandler class >> simulateMiddleClick [

	^ SimulateMiddleButton ifNil: [ SimulateMiddleButton := true ]
]

{ #category : 'event testing' }
OSWindowMorphicEventHandler class >> simulateMiddleClick: aValue [

	SimulateMiddleButton := aValue
]

{ #category : 'asserting' }
OSWindowMorphicEventHandler class >> simulateMiddleClickSettingOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #simulateMiddleClick)
		target: self;
		default: true;
		parent: #morphic;
		label: 'Simulate Middle Mouse Button';
		description: 'Sets if we are simulating the middle button by doing Alt + Left button'
]

{ #category : 'private' }
OSWindowMorphicEventHandler >> activeHand [
	^ self morphicWorld activeHand
]

{ #category : 'converting' }
OSWindowMorphicEventHandler >> convertButtonFromEvent: anEvent [

	anEvent button = 1 ifTrue: [ ^ MouseButtonEvent redButton ].
	anEvent button = 2 ifTrue: [ ^ MouseButtonEvent blueButton ].
	anEvent button = 3 ifTrue: [ ^ MouseButtonEvent yellowButton ]
]

{ #category : 'converting' }
OSWindowMorphicEventHandler >> convertModifiers: modifiers [
	| buttons |
	buttons := 0.

	modifiers alt ifTrue: [ buttons := buttons | UserInputEvent altKeyMask ].
	modifiers ctrl ifTrue: [ buttons := buttons | UserInputEvent ctrlKeyMask ].
	modifiers shift ifTrue: [ buttons := buttons | UserInputEvent shiftKeyMask ].
	modifiers cmd ifTrue: [ buttons := buttons | UserInputEvent cmdKeyMask ].

	modifiers buttons button1 ifTrue:  [ buttons := buttons | MouseButtonEvent redButton ].
	modifiers buttons button2 ifTrue:  [ buttons := buttons | MouseButtonEvent blueButton ].
	modifiers buttons button3 ifTrue:  [ buttons := buttons | MouseButtonEvent yellowButton ].
	^ buttons
]

{ #category : 'converting' }
OSWindowMorphicEventHandler >> convertPosition: aPosition [
	morphicWorld ifNil: [ ^ aPosition ].
	^ morphicWorld worldState worldRenderer convertWindowMouseEventPosition: aPosition
]

{ #category : 'converting' }
OSWindowMorphicEventHandler >> convertText: aString [

	"I convert the string if some well-known problematic characters are found.
	Currently I am converting the 'Modifier Letter Circumflex Accent (U+02C6) without a modifying character to the circunflex (U+005E).
	See issue #16132'"
	
	^ aString copy replaceAll: (Character value: 16r02C6) with: $^.
]

{ #category : 'events' }
OSWindowMorphicEventHandler >> dispatchMorphicEvent: anEvent [
	morphicWorld defer: [
		(morphicWorld activeHand isNotNil and: [ anEvent hand isNotNil ]) ifTrue: [
			morphicWorld activeHand handleEvent: anEvent
		]
	]
]

{ #category : 'events' }
OSWindowMorphicEventHandler >> handleEvent: anEvent [

	"convert the event to morphic one, and dispatch it whatever..."
	| morphicEvent |

	morphicEvent := anEvent accept: self.
	morphicEvent isMorphicEvent ifFalse: [ ^ self ].

	self dispatchMorphicEvent: morphicEvent
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> mapSymbolToKeyValue: symbol [
	^ SymbolCharacterMapping at: symbol ifAbsent: [
		 "Don't allow symbol values outside the unicode range"
		 symbol >= 16r400000 ifTrue: [ 0 ] ifFalse: [ symbol ] ]
]

{ #category : 'accessing' }
OSWindowMorphicEventHandler >> morphicWorld [
	^ morphicWorld
]

{ #category : 'accessing' }
OSWindowMorphicEventHandler >> morphicWorld: aMorphicWorld [
	morphicWorld := aMorphicWorld
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitKeyDownEvent: anEvent [
	| keyEvent mods |

	mods := anEvent modifiers.
	keyEvent := KeyboardEvent new
		setType: #keyDown
		buttons: (self convertModifiers: mods)
		position: (self convertPosition: anEvent position)
		keyValue: (self mapSymbolToKeyValue: anEvent symbol)
		charCode: (self mapSymbolToKeyValue: anEvent symbol)
		hand: self activeHand
		stamp: Time millisecondClockValue.
	keyEvent
		scanCode: anEvent scanCode;
		key: (OSKeySymbols mapKeySymbolValueToKeyboardKey: anEvent symbol);
		isRepeat: anEvent repeat = 1.
	self dispatchMorphicEvent: keyEvent
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitKeyUpEvent: anEvent [
	| keyEvent |
	keyEvent := KeyboardEvent new
		setType: #keyUp
		buttons: (self convertModifiers: anEvent modifiers)
		position: (self convertPosition: anEvent position)
		keyValue: (self mapSymbolToKeyValue: anEvent symbol)
		charCode: (self mapSymbolToKeyValue: anEvent symbol)
		hand: self activeHand
		stamp: Time millisecondClockValue.
	keyEvent
		scanCode: anEvent scanCode;
		key: (OSKeySymbols mapKeySymbolValueToKeyboardKey: anEvent symbol);
		isRepeat: anEvent repeat = 1.
	^ keyEvent
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitMouseButtonPressEvent: anEvent [
	anEvent isWheel ifTrue: [
		^ MouseWheelEvent new
			setType: #mouseWheel
			position: (self convertPosition: anEvent position)
			direction: anEvent wheelDirection
			buttons: (self convertModifiers: anEvent modifiers)
			hand: self activeHand
			stamp: Time millisecondClockValue ].

	^ MouseButtonEvent new
		setType: #mouseDown
		position: (self convertPosition: anEvent position)
		which: (self convertButtonFromEvent: anEvent)
		buttons: (self convertModifiers: anEvent modifiers) | (self convertButtonFromEvent: anEvent)
		hand: self activeHand
		stamp: Time millisecondClockValue
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitMouseButtonReleaseEvent: anEvent [
	anEvent isWheel ifTrue: [ ^ nil ].

	^ MouseButtonEvent new
		setType: #mouseUp
		position: (self convertPosition: anEvent position)
		which: (self convertButtonFromEvent: anEvent)
		buttons: (self convertModifiers: anEvent modifiers)
		hand: self activeHand
		stamp: Time millisecondClockValue
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitMouseMoveEvent: anEvent [
	| oldPos newPos |
	oldPos := morphicWorld activeHand ifNil: [ 0@0 ] ifNotNil: [:hand | hand position ].
	morphicWorld beCursorOwner.
	newPos := self convertPosition: anEvent position.
	^ MouseMoveEvent basicNew
		setType: #mouseMove
		startPoint: oldPos
		endPoint: newPos
		trail: { oldPos. newPos }
		buttons: (self convertModifiers: anEvent modifiers)
		hand: self activeHand
		stamp: Time millisecondClockValue
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitMouseWheelEvent: anEvent [

	| vertical horizontal direction |
	vertical := anEvent scrollVertical.
	horizontal := anEvent scrollHorizontal.

	direction := vertical isZero
		             ifTrue: [
			             horizontal > 0
				             ifTrue: [ Character arrowLeft ]
				             ifFalse: [ Character arrowRight ] ]
		             ifFalse: [
			             vertical > 0
				             ifTrue: [ Character arrowUp ]
				             ifFalse: [ Character arrowDown ] ].

	^ MouseWheelEvent new
		  setType: #mouseWheel
		  position: (self convertPosition: anEvent position)
		  direction: direction
		  buttons: (self convertModifiers: anEvent modifiers)
		  hand: self activeHand
		  stamp: Time millisecondClockValue
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitTextEditingEvent: anEvent [

	| keyEvent |
	keyEvent := TextEditionEvent new
		setHand: self activeHand;
		setPosition: (self convertPosition: anEvent position);
		text: anEvent text;
		start: anEvent start;
		length: anEvent length;
		wasHandled: false.
	^ keyEvent
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitTextInputEvent: anEvent [
	anEvent text ifNil: [ ^ nil ].
	
	(anEvent text size = 1 and: [ anEvent text first codePoint < 128 ])
		ifTrue: [
			| char mods |
			char := anEvent text first.
			char ifNil: [ ^ nil ].
			mods := anEvent modifiers.
			"If a modifier key is pressed the keystroke event is handled by #visitMouseDownEvent:"
			(mods alt or: [ mods ctrl or: [ mods cmd ] ]) ifTrue: [ ^ nil ].

			^ KeyboardEvent new
				  setType: #keystroke
				  buttons: (self convertModifiers: anEvent modifiers)
				  position: (self convertPosition: anEvent position)
				  keyValue: char charCode
				  charCode: char charCode
				  hand: self activeHand
				  stamp: Time millisecondClockValue ]
		ifFalse: [
			^ TextInputEvent new
				  setHand: self activeHand;
				  setPosition: (self convertPosition: anEvent position);
				  text: (self convertText: anEvent text);
				  wasHandled: false ]
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitUnknownEvent: anEvent [
	self
		traceCr: ('Unknown event: <1s>' expandMacrosWith: anEvent data printString)
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitWindowCloseEvent: anEvent [
	anEvent suppressDefaultAction.

	morphicWorld worldState worldRenderer handleOSWindowCloseEvent: anEvent
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitWindowDropEvent: anEvent [

	| dropEvent |

	"The OSWindow drop events are generated one-per file (unlike the case the non-headless VM)"

	dropEvent := DropFilesEvent new
				setPosition: self activeHand position
				contents: 1
				hand: self activeHand;
				fileNames: { anEvent filePath }.

	^ dropEvent
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitWindowExposeEvent: anEvent [
	"Completely redraw world when visible part of window changes to prevent some graphical glitches"
    morphicWorld worldState doFullRepaint
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitWindowResizeEvent: anEvent [

	"window resized"
	morphicWorld worldState worldRenderer checkForNewScreenSize
]

{ #category : 'visiting' }
OSWindowMorphicEventHandler >> visitWindowResolutionChangeEvent: anEvent [

	morphicWorld worldState updateToNewResolution: anEvent
]
