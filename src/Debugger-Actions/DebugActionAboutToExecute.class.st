"
A DebugActionAboutToExecute is an event send to the debugger before a debugging action is executed.
"
Class {
	#name : #DebugActionAboutToExecute,
	#superclass : #Announcement,
	#instVars : [
		'debuggingAction'
	],
	#category : #'Debugger-Actions-Announcements'
}

{ #category : #'instance creation' }
DebugActionAboutToExecute class >> forDebuggingAction: anAction [

	^ self new
		action: anAction
]

{ #category : #accessing }
DebugActionAboutToExecute >> action [

	^ debuggingAction
]

{ #category : #accessing }
DebugActionAboutToExecute >> action: aDebuggingAction [

	debuggingAction := aDebuggingAction
]
