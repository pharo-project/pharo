"
I check which keys the taskbar handler takes, on a world of my own.
"
Class {
	#name : 'TaskbarRootEventHandlerTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Widgets-Taskbar-Tests',
	#package : 'Morphic-Widgets-Taskbar-Tests'
}

{ #category : 'helpers' }
TaskbarRootEventHandlerTest >> cmdShiftAlt: aCharacter hand: aHand [
	"A key down of aCharacter with command, shift and alt pressed"

	^ (KeyboardEvent new
		   setType: #keyDown
		   buttons: (UserInputEvent classVarNamed: #CMD_KEY_MASK)
			   | (UserInputEvent classVarNamed: #SHIFT_KEY_MASK)
			   | (UserInputEvent classVarNamed: #ALT_KEY_MASK)
		   position: 0 @ 0
		   keyValue: aCharacter asUppercase asInteger
		   charCode: aCharacter asInteger
		   hand: aHand
		   stamp: 0)
		  key: aCharacter asKeyCombination key;
		  yourself
]

{ #category : 'helpers' }
TaskbarRootEventHandlerTest >> keyDown: aCharacter [

	^ KeyboardEvent new
		  setType: #keyDown
		  buttons: 0
		  position: 0 @ 0
		  keyValue: aCharacter asInteger
		  charCode: aCharacter asInteger
		  hand: nil
		  stamp: 0
]

{ #category : 'tests' }
TaskbarRootEventHandlerTest >> testLetsOtherKeysReachTheFocus [

	self deny: (TaskbarRootEventHandler new handleEvent: (self keyDown: $a) in: WorldMorph new)
]

{ #category : 'tests' }
TaskbarRootEventHandlerTest >> testMovesTheTaskOfTheFocusedWindow [

	| world first second taskbar hand order |
	world := WorldMorph new.
	first := SystemWindow labelled: 'first'.
	second := SystemWindow labelled: 'second'.
	world
		addMorph: first;
		addMorph: second.
	taskbar := TaskbarMorph new.
	world addMorph: taskbar.
	taskbar updateTasks.
	order := taskbar orderedTasks collect: [ :task | task morph ] as: Array.
	hand := HandMorph new.
	world addHand: hand.
	hand newKeyboardFocus: order first.

	self assert: (TaskbarRootEventHandler new handleEvent: (self cmdShiftAlt: $l hand: hand) in: world).

	self assert: (taskbar orderedTasks collect: [ :task | task morph ] as: Array) equals: order reversed
]
