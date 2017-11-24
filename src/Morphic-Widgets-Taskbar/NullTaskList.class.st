"
I stand in when the task list is not open in the world.
"
Class {
	#name : #NullTaskList,
	#superclass : #Object,
	#instVars : [
		'world'
	],
	#category : #'Morphic-Widgets-Taskbar'
}

{ #category : #'instance creation' }
NullTaskList class >> in: world [

	^ self new
		world: world;
		yourself.
]

{ #category : #'event handling' }
NullTaskList >> handleEvent: aKeyboardEvent [ 

	aKeyboardEvent isWindowNavigation ifFalse: [ ^ false ].
	world theme openTaskListIn: world from: aKeyboardEvent.
	^ true.
]

{ #category : #private }
NullTaskList >> world: aWorldMorph [ 

	world := aWorldMorph.
]
