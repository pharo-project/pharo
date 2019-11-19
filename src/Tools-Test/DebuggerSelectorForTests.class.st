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
		'debuggerSelectionAttempts',
		'debuggerClasses'
	],
	#category : #'Tools-Test-Debugger'
}

{ #category : #'as yet unclassified' }
DebuggerSelectorForTests >> clearDebuggerClasses [
	debuggerClasses := OrderedCollection new
]

{ #category : #initialization }
DebuggerSelectorForTests >> debuggerClasses [
	^ debuggerClasses

]

{ #category : #accessing }
DebuggerSelectorForTests >> debuggerSelectionAttempts [
	^ debuggerSelectionAttempts
]

{ #category : #initialization }
DebuggerSelectorForTests >> getDebuggerClasses [
	"The superclass' method, that looks for all debugger classes in the system, is repalced with a simple collection for testing purposes. The tests manually put test debugger classes in the collection, so that they can specify which debugger classes the debuggerSelector that is tested can work with. Otherwise, the tests of DebuggerSelector can be impacted by the presence or absence of regular debugger classes."
	^ debuggerClasses
]

{ #category : #initialization }
DebuggerSelectorForTests >> getDebuggerClassesReal [
	"Since #getDebuggerClasses is overriden for testing purposes in this class, we need another way to test the normal implementation of #getDebuggerClass"
	^ super getDebuggerClasses
]

{ #category : #initialization }
DebuggerSelectorForTests >> ignoreDummyDebuggerForTests [
	^ false
]

{ #category : #'reflective operations' }
DebuggerSelectorForTests >> initialize [
	super initialize.
	debuggerSelectionAttempts := OrderedCollection new.
	debuggerClasses := OrderedCollection new.
]

{ #category : #helper }
DebuggerSelectorForTests >> selectDebuggerAndOpen: a withFullView: b andNotification: c [
	super selectDebuggerAndOpen: a withFullView: b andNotification: c
]

{ #category : #helper }
DebuggerSelectorForTests >> tryToOpenDebugSession: aDebugSession withDebuggerClass: aDebuggerClass withFullView: aBool andNotification: aString [
	"This method is the same as that of the superclass, with the difference that it logs which debuggers this class tried to open in the debuggerSelectionAttempts variable, along with whether the opening succeeded or not"
	[aDebuggerClass openOn: aDebugSession withFullView: aBool andNotification: aString. self debuggerSelectionAttempts add: (DebuggerSelectionRecordUnit newOnDebuggerClass: aDebuggerClass withSuccess: true). ^ true ] on: Error do: [ self debuggerSelectionAttempts add: (DebuggerSelectionRecordUnit newOnDebuggerClass: aDebuggerClass withSuccess: false). ^false ]
]
