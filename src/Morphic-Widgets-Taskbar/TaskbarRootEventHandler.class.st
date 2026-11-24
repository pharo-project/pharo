"
I give a world its taskbar keys: Alt+Tab (and Alt+Shift+Tab) open the task list or move its selection, and Cmd+Shift+Alt+K / Cmd+Shift+Alt+L move the task of the focused window left or right in the taskbar.

I take these keys before the keyboard focus, so that a focused text editor does not consume them.

Add me to the world: World addRootEventHandler: TaskbarRootEventHandler new.
"
Class {
	#name : 'TaskbarRootEventHandler',
	#superclass : 'MorphRootEventHandler',
	#category : 'Morphic-Widgets-Taskbar',
	#package : 'Morphic-Widgets-Taskbar'
}

{ #category : 'event handling' }
TaskbarRootEventHandler >> handleEvent: anEvent in: aWorld [
	"Take the taskbar keys before they reach the keyboard focus holder: a focused text editor would consume them"

	(anEvent isKeyboard and: [ anEvent isKeyDown ]) ifFalse: [ ^ false ].
	(self takeKeyDown: anEvent in: aWorld) ifFalse: [ ^ false ].
	"The key press that follows the key down would still reach the focus holder"
	anEvent supressNextKeyPress: true.
	^ true
]

{ #category : 'private' }
TaskbarRootEventHandler >> moveTaskOfFocusedWindowBy: offset for: aKeyboardEvent [
	"Move the task of the window that has the keyboard focus. Answer whether there was one to move"

	| window |
	window := aKeyboardEvent hand keyboardFocus ifNotNil: [ :focus | focus window ].
	window ifNil: [ ^ false ].
	window worldTaskbar ifNil: [ ^ false ] ifNotNil: [ :taskbar | taskbar moveTaskOf: window by: offset ].
	^ true
]

{ #category : 'private' }
TaskbarRootEventHandler >> takeKeyDown: aKeyboardEvent in: aWorld [
	"Answer whether aKeyboardEvent is a taskbar key, after acting on it"

	(TaskListMorph isNavigationEvent: aKeyboardEvent) ifTrue: [
		aWorld taskList handleEvent: aKeyboardEvent.
		^ true ].
	aKeyboardEvent asKeyCombination = $k meta shift alt ifTrue: [ ^ self moveTaskOfFocusedWindowBy: -1 for: aKeyboardEvent ].
	aKeyboardEvent asKeyCombination = $l meta shift alt ifTrue: [ ^ self moveTaskOfFocusedWindowBy: 1 for: aKeyboardEvent ].
	^ false
]
