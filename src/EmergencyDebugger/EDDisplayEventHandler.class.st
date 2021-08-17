"
An event handler for the emergency debugger, it will just handle a few things: 
- character entries, and
- the window close button
"
Class {
	#name : #EDDisplayEventHandler,
	#superclass : #OSWindowEventVisitor,
	#instVars : [
		'interface'
	],
	#category : #'EmergencyDebugger-View'
}

{ #category : #'instance creation' }
EDDisplayEventHandler class >> on: anInterface [

	^ self new 
		interface: anInterface;
		yourself
]

{ #category : #accessing }
EDDisplayEventHandler >> interface [

	^ interface
]

{ #category : #accessing }
EDDisplayEventHandler >> interface: anObject [

	interface := anObject
]

{ #category : #visiting }
EDDisplayEventHandler >> visitKeyDownEvent: event [

	event character ifNil: [ ^ self ].	
	interface pushChar: event character
]

{ #category : #visiting }
EDDisplayEventHandler >> visitTextInputEvent: anEvent [
	| char mods |

	anEvent text ifNil: [ ^ nil ].

	char := anEvent text first.
	char ifNil: [ ^ nil ].
	mods := anEvent modifiers.
	"If a modifier key is pressed the keystroke event is handled by #visitMouseDownEvent:"
	(mods alt or: [ mods ctrl or: [ mods cmd ] ])
		ifTrue: [ ^ nil ].

	interface pushChar: char
]

{ #category : #visiting }
EDDisplayEventHandler >> visitWindowCloseEvent: anEvent [

	anEvent suppressDefaultAction.
	interface closeWindow
]
