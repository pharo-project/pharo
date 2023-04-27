"
is same of taskBarMorph but just for the manager
"
Class {
	#name : #TaskbarMorphForManager,
	#superclass : #TaskbarMorph,
	#instVars : [
		'manager'
	],
	#category : #'Morphic-Widgets-Taskbar'
}

{ #category : #'instance creation' }
TaskbarMorphForManager class >> newByManager: aManager [

	| taskbar |
	taskbar := self new.
	taskbar manager: aManager .
	^ taskbar 
]

{ #category : #accessing }
TaskbarMorphForManager >> manager: aManager [

	manager := aManager 
]

{ #category : #taskbar }
TaskbarMorphForManager >> windows [

	^ manager allWindow
]
