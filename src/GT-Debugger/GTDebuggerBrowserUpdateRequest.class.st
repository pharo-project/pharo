"
A GTDebuggerBrowserUpdateRequest is a wrapper for a DebugActionExecuted event. It is sent to the debugger after the DebugActionExecuted event was handled by the debugger.
"
Class {
	#name : #GTDebuggerBrowserUpdateRequest,
	#superclass : #Announcement,
	#instVars : [
		'debugActionExecutedAnnouncement'
	],
	#category : #'GT-Debugger-UI'
}

{ #category : #'instance creation' }
GTDebuggerBrowserUpdateRequest class >> from: anAnnouncement [
	^ self new from: anAnnouncement
]

{ #category : #testing }
GTDebuggerBrowserUpdateRequest class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GTDebuggerBrowserUpdateRequest >> debugActionExecutedAnnouncement [
	^ debugActionExecutedAnnouncement
]

{ #category : #initialization }
GTDebuggerBrowserUpdateRequest >> from: anAnnouncement [
	debugActionExecutedAnnouncement := anAnnouncement
]
