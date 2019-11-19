"
I am a copy of class DebuggerSelectorDecoratorForTest. This comment is copied from there, and might not be entirely accurate

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
	debuggerSelectionAttempts:		<Object>
	decoratedDebuggerSelector:		<Object>


    Implementation Points
"
Class {
	#name : #DebuggerSelectorForTests,
	#superclass : #DebuggerSelector,
	#instVars : [
		'debuggerSelectionAttempts'
	],
	#category : #'Tools-Test-Debugger'
}

{ #category : #'instance creation' }
DebuggerSelectorForTests class >> newOn: aDebuggerSelector [
	| instance |
	instance := self new.
	instance decoratedDebuggerSelector: aDebuggerSelector.
	^ instance
]

{ #category : #accessing }
DebuggerSelectorForTests >> debuggerSelectionAttempts [
	^ debuggerSelectionAttempts
]

{ #category : #initialization }
DebuggerSelectorForTests >> ignoreDummyDebuggerForTests [
	^ false
]

{ #category : #'reflective operations' }
DebuggerSelectorForTests >> initialize [
	super initialize.
	debuggerSelectionAttempts := OrderedCollection new.
]

{ #category : #helper }
DebuggerSelectorForTests >> selectDebuggerAndOpen: a withFullView: b andNotification: c [
	self inform: 'DSFT'.
	super selectDebuggerAndOpen: a withFullView: b andNotification: c
]

{ #category : #helper }
DebuggerSelectorForTests >> tryToOpenDebugSession: aDebugSession withDebuggerClass: aDebuggerClass withFullView: aBool andNotification: aString [
	"Tries to open @aDebugSession with debugger @aDebuggerClass. Record this attempt in debuggerSelectionAttempts, with success = true if it succeeded, or success = false if it signaled an exception"
	self inform: 'called'.
	[aDebuggerClass openOn: aDebugSession withFullView: aBool andNotification: aString. self debuggerSelectionAttempts add: (DebuggerSelectionRecordUnit newOnDebuggerClass: aDebuggerClass withSuccess: true). ^ true ] on: Error do: [ self debuggerSelectionAttempts add: (DebuggerSelectionRecordUnit newOnDebuggerClass: aDebuggerClass withSuccess: true). ^false ]
]
