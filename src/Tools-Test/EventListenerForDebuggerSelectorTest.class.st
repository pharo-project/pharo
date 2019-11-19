"
I am a copy of class EventListenerForDebugSessionStorageTest. This comment is copied from there, and might not be entirely accurate

Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, ""I represent a paragraph of text"".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	nbOfDebugSessionStorageLimitUpdateEventReceived:		<Object>
	nbOfDebugSessionStorageUpdateEventReceived:		<Object>


    Implementation Points
"
Class {
	#name : #EventListenerForDebuggerSelectorTest,
	#superclass : #Object,
	#instVars : [
		'nbOfMainDebuggerClassUpdateEventReceived'
	],
	#category : #'Tools-Test-Debugger'
}

{ #category : #'react to events' }
EventListenerForDebuggerSelectorTest >> initialize [
	super initialize.
	nbOfMainDebuggerClassUpdateEventReceived := 0
]

{ #category : #accessing }
EventListenerForDebuggerSelectorTest >> nbOfMainDebuggerClassUpdateEventReceived [
	^ nbOfMainDebuggerClassUpdateEventReceived
]

{ #category : #'react to events' }
EventListenerForDebuggerSelectorTest >> onMainDebuggerClassUpdate [
	nbOfMainDebuggerClassUpdateEventReceived := nbOfMainDebuggerClassUpdateEventReceived + 1
]
