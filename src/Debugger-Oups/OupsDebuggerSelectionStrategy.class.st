"
I am the abstract super class of **Oups**' debugger selection strategies. My subclasses are responsible for selecting a debugger from a given list of debuggers, and for opening a given error with it.

**Oups** has a setting to define which strategy it should use to find a debugger for the error whose debugging was requested.
The debugger infrastructure implements a default strategy `OupsDebuggerSelector` which is interchangeable with user-defined strategies.
"
Class {
	#name : 'OupsDebuggerSelectionStrategy',
	#superclass : 'Object',
	#instVars : [
		'debuggers',
		'handled',
		'error'
	],
	#classInstVars : [
		'debuggerSelectionStrategy'
	],
	#category : 'Debugger-Oups-Strategies',
	#package : 'Debugger-Oups',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
OupsDebuggerSelectionStrategy class >> debuggerSelectionStrategy [

	^ debuggerSelectionStrategy ifNil: [
		  debuggerSelectionStrategy := self defaultDebuggerSelectionStrategy ]
]

{ #category : 'accessing' }
OupsDebuggerSelectionStrategy class >> debuggerSelectionStrategy: aClass [
	debuggerSelectionStrategy := aClass
]

{ #category : 'accessing' }
OupsDebuggerSelectionStrategy class >> defaultDebuggerSelectionStrategy [
	^OupsDebuggerSelector
]

{ #category : 'instance' }
OupsDebuggerSelectionStrategy class >> with: aCollection [
	^ self new
		debuggers: aCollection;
		yourself
]

{ #category : 'accessing' }
OupsDebuggerSelectionStrategy >> debuggers [
	^ debuggers
]

{ #category : 'accessing' }
OupsDebuggerSelectionStrategy >> debuggers: aCollection [
	debuggers := aCollection
]

{ #category : 'error handling' }
OupsDebuggerSelectionStrategy >> error [
	^error
]

{ #category : 'accessing' }
OupsDebuggerSelectionStrategy >> handled [
	^handled ifNil:[handled := false]
]

{ #category : 'accessing' }
OupsDebuggerSelectionStrategy >> hasErrors [

	^ error isNotNil
]

{ #category : 'error handling' }
OupsDebuggerSelectionStrategy >> logErrorsForSession: aDebugSession [

	error ifNotNil: [ "There was an error while opening the debugger, we log that error"
		DebuggerEmmergencyLogger new
			logError: error
			forSession: aDebugSession ].

	"We log the original error."
	DebuggerEmmergencyLogger new
		logError: aDebugSession exception
		forSession: aDebugSession
]

{ #category : 'iterating' }
OupsDebuggerSelectionStrategy >> nextDebugger [
	^ self subclassResponsibility
]

{ #category : 'debuggers' }
OupsDebuggerSelectionStrategy >> openDebuggerForSession: aDebugSession [
	self subclassResponsibility
]
