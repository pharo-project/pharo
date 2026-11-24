"
I check which keys the task list handler takes, on a world of my own.
"
Class {
	#name : 'TaskListRootEventHandlerTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Widgets-Taskbar-Tests',
	#package : 'Morphic-Widgets-Taskbar-Tests'
}

{ #category : 'helpers' }
TaskListRootEventHandlerTest >> keyDown: aCharacter [

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
TaskListRootEventHandlerTest >> testLetsOtherKeysReachTheFocus [

	self deny: (TaskListRootEventHandler new handleEvent: (self keyDown: $a) in: WorldMorph new)
]

{ #category : 'tests' }
TaskListRootEventHandlerTest >> testOtherKeysDoNotNavigate [

	self deny: (TaskListRootEventHandler new handleNavigationKey: (self keyDown: $a) for: WorldMorph new)
]
