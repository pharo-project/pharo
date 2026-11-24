"
I switch between the windows of a world with Alt+Tab (and Alt+Shift+Tab): I open the task list, or move its selection.

I take Alt+Tab before the keyboard focus, so that a focused text editor does not consume it.

Add me to the world: World addRootEventHandler: TaskListRootEventHandler new.
"
Class {
	#name : 'TaskListRootEventHandler',
	#superclass : 'MorphRootEventHandler',
	#category : 'Morphic-Widgets-Taskbar',
	#package : 'Morphic-Widgets-Taskbar'
}

{ #category : 'event handling' }
TaskListRootEventHandler >> handleEvent: anEvent in: aWorld [
	"Take Alt+Tab before it reaches the keyboard focus holder: a focused text editor would consume it"

	(anEvent isKeyboard and: [ anEvent isKeyDown and: [ TaskListMorph isNavigationEvent: anEvent ] ]) ifFalse: [ ^ false ].
	aWorld navigationKey: anEvent.
	"The key press that follows the key down would still reach the focus holder"
	anEvent supressNextKeyPress: true.
	^ true
]

{ #category : 'event handling' }
TaskListRootEventHandler >> handleNavigationKey: aKeyboardEvent for: aWorld [

	^ aWorld taskList handleEvent: aKeyboardEvent
]
