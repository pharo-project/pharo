Class {
	#name : #DebuggerSelectorDecoratorForTest,
	#superclass : #Object,
	#instVars : [
		'decoratedDebuggerSelector',
		'debuggerSelectionAttempts'
	],
	#category : #'Tools-Test-Debugger'
}

{ #category : #'instance creation' }
DebuggerSelectorDecoratorForTest class >> newOn: aDebuggerSelector [
	| instance |
	instance := self new.
	instance decoratedDebuggerSelector: aDebuggerSelector.
	^ instance
]

{ #category : #accessing }
DebuggerSelectorDecoratorForTest >> debuggerSelectionAttempts [
	^ debuggerSelectionAttempts
]

{ #category : #accessing }
DebuggerSelectorDecoratorForTest >> decoratedDebuggerSelector: aDebuggerSelector [
	decoratedDebuggerSelector := aDebuggerSelector
]

{ #category : #'reflective operations' }
DebuggerSelectorDecoratorForTest >> doesNotUnderstand: aMessage [
	aMessage lookupClass: decoratedDebuggerSelector class.
	^ aMessage sentTo: decoratedDebuggerSelector
]

{ #category : #initialization }
DebuggerSelectorDecoratorForTest >> ignoreDummyDebuggerForTests [
	^ false
]

{ #category : #'reflective operations' }
DebuggerSelectorDecoratorForTest >> initialize [
	super initialize.
	debuggerSelectionAttempts := OrderedCollection new.
]

{ #category : #helper }
DebuggerSelectorDecoratorForTest >> tryToOpenDebugSession: aDebugSession withDebuggerClass: aDebuggerClass withFullView: aBool andNotification: aString [
	"Tries to open @aDebugSession with debugger @aDebuggerClass. Record this attempt in debuggerSelectionAttempts, with success = true if it succeeded, or success = false if it signaled an exception"
	[aDebuggerClass openOn: aDebugSession withFullView: aBool andNotification: aString. self debuggerSelectionAttempts add: (DebuggerSelectionRecordUnit newOnDebuggerClass: aDebuggerClass withSuccess: true) ] on: Error do: [ self debuggerSelectionAttempts add: (DebuggerSelectionRecordUnit newOnDebuggerClass: aDebuggerClass withSuccess: true) ]
]
