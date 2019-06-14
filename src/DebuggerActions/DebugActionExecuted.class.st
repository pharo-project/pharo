"
A DebugActionExecuted is an event send to the debugger after a debugging action is executed.

Instance Variables
	actionId:		<Object>

actionId
	- xxxxx

"
Class {
	#name : #DebugActionExecuted,
	#superclass : #Announcement,
	#instVars : [
		'debuggingAction'
	],
	#category : #'Debugger-Actions-Announcements'
}

{ #category : #'instance creation' }
DebugActionExecuted class >> forDebuggingAction: anAction [

	^ self new
		action: anAction
]

{ #category : #accessing }
DebugActionExecuted >> action [

	^ debuggingAction
]

{ #category : #accessing }
DebugActionExecuted >> action: aDebuggingAction [

	debuggingAction := aDebuggingAction
]
