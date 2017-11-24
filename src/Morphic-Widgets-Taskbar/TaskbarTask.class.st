"
Holds the state of a taskbar/tasklist entry.
"
Class {
	#name : #TaskbarTask,
	#superclass : #Object,
	#instVars : [
		'morph',
		'state',
		'label',
		'icon'
	],
	#category : #'Morphic-Widgets-Taskbar'
}

{ #category : #'intance creation' }
TaskbarTask class >> morph: aMorph state: aSymbol icon: aForm label: aString [
	"Answer a new instance of the receiver with the given parameters."

	^self new
		morph: aMorph;
		state: aSymbol;
		icon: aForm;
		label: aString
]

{ #category : #comparing }
TaskbarTask >> = aTaskbarTask [
	"Answer whether equal."

	^self species = aTaskbarTask species
		and: [self morph == aTaskbarTask morph
		and: [self state == aTaskbarTask state
		and: [self icon = aTaskbarTask icon
		and: [self label = aTaskbarTask label]]]]
]

{ #category : #activation }
TaskbarTask >> activate [
	"Activate the task."
	
	(self morph respondsTo: #restoreAndActivate)
		ifTrue: [self morph restoreAndActivate]
]

{ #category : #'user-interface' }
TaskbarTask >> buttonClickedForTaskList: aTasklist [
	"Notify the tasklist."
	
	aTasklist taskClicked: self
]

{ #category : #comparing }
TaskbarTask >> hash [
	"Hash is implemented because #= is implemented."
	
	^self morph hash
		bitXor: (self state hash
		bitXor: (self icon hash
		bitXor: self label hash))
]

{ #category : #accessing }
TaskbarTask >> icon [
	"Answer the value of icon"

	^ icon
]

{ #category : #accessing }
TaskbarTask >> icon: anObject [
	"Set the value of icon"

	icon := anObject
]

{ #category : #testing }
TaskbarTask >> isActive [
	"Answer whether the task is active."

	^self state == #active
]

{ #category : #testing }
TaskbarTask >> isMinimized [
	"Answer whether the task is minimized."

	^self state == #minimized
]

{ #category : #accessing }
TaskbarTask >> label [
	"Answer the value of label"

	^ label
]

{ #category : #accessing }
TaskbarTask >> label: anObject [
	"Set the value of label"

	label := anObject
]

{ #category : #accessing }
TaskbarTask >> morph [
	"Answer the value of morph"

	^ morph
]

{ #category : #accessing }
TaskbarTask >> morph: anObject [
	"Set the value of morph"

	morph := anObject
]

{ #category : #accessing }
TaskbarTask >> state [
	"Answer the value of state"

	^ state
]

{ #category : #accessing }
TaskbarTask >> state: anObject [
	"Set the value of state"

	state := anObject
]

{ #category : #'user-interface' }
TaskbarTask >> taskListButtonFor: aTasklist [
	"Answer a button for the task."
	
	^self theme
		newTaskListButtonIn: aTasklist
		for: self
]

{ #category : #'user-interface' }
TaskbarTask >> taskbarButtonFor: aTaskBar [
	"Answer a button for the task."
	
	^self morph taskbarButtonFor: aTaskBar
]

{ #category : #accessing }
TaskbarTask >> theme [
	^ Smalltalk ui theme
]
