"
I take as input a sorted list of debuggers and a debug session.
I will try to open the debug session with the first applicable debugger in this list.

If this first applicable debugger encounters an error, I will either try the next applicable debugger or if I am configured to handle debugger errors (see handlesDebuggerErrors:) I will raise a DebuggerFailure exception.

When I try to open a debug session created from a DebuggerFailure, I exclude the debugger that produced the debugger failure from my debugger list.

When I succeeded to open a debugger, my instance variable ""handled"" becomes true. 
If I failed to find any debugger, or to open the debug session with any debugger, ""handled"" stays false. This is the responsibility of my clients to deal with this situation (i.e., the system was unable to open a debugger).
"
Class {
	#name : 'OupsDebuggerSelector',
	#superclass : 'OupsDebuggerSelectionStrategy',
	#instVars : [
		'handlesDebuggerErrors'
	],
	#classInstVars : [
		'handleDebuggerErrors'
	],
	#category : 'Debugger-Oups-Strategies',
	#package : 'Debugger-Oups',
	#tag : 'Strategies'
}

{ #category : 'settings' }
OupsDebuggerSelector class >> handleDebuggerErrors [
	^ handleDebuggerErrors ifNil: [ handleDebuggerErrors := false ]
]

{ #category : 'settings' }
OupsDebuggerSelector class >> handleDebuggerErrors: aBoolean [
	handleDebuggerErrors := aBoolean
]

{ #category : 'settings' }
OupsDebuggerSelector class >> handleDebuggerErrorsSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #handleDebuggerErrors)
		label: 'Handle debugger errors';
		target: self;
		default: false;
		parent: #debugging;
		description: 'Try to debug debugger errors when they occur.'
]

{ #category : 'accessing' }
OupsDebuggerSelector >> debuggers: aCollection [
	debuggers := Stack newFrom: aCollection
]

{ #category : 'accessing' }
OupsDebuggerSelector >> handlesDebuggerErrors [
	^ handlesDebuggerErrors
		ifNil: [ handlesDebuggerErrors := self class handleDebuggerErrors ]
]

{ #category : 'accessing' }
OupsDebuggerSelector >> handlesDebuggerErrors: aBoolean [
	handlesDebuggerErrors := aBoolean
]

{ #category : 'iterating' }
OupsDebuggerSelector >> nextDebugger [
	^ debuggers pop
]

{ #category : 'debuggers' }
OupsDebuggerSelector >> nextDebuggerForSession: aDebugSession [

	| debugger |
	debugger := self nextDebugger.
	[
	(debugger handlesDebugSession: aDebugSession) and: [
		aDebugSession exception isHandleableBy: debugger ] ] whileFalse: [
		debugger := self nextDebugger ].
	^ debugger
]

{ #category : 'debuggers' }
OupsDebuggerSelector >> openDebuggerForSession: aDebugSession [

	<debuggerCompleteToSender>
	handled := false.
	[ handled or: [ self debuggers isEmpty ] ] whileFalse: [
		self
			tryOpen: aDebugSession
			with: (self nextDebuggerForSession: aDebugSession) ].
	handled ifTrue: [ ^ self ].

	self logErrorsForSession: aDebugSession
]

{ #category : 'debuggers' }
OupsDebuggerSelector >> tryOpen: aDebugSession with: aDebugger [

	<debuggerCompleteToSender>
	[
	aDebugger debugSession: aDebugSession.
	handled := true ]
		on: Error , UnhandledException
		do: [ :err |
			error := err.
			(self handlesDebuggerErrors and: [
				 aDebugSession exception isDebuggerFailure not ]) ifTrue: [
				(OupsDebuggerFailure of: aDebugger on: err) signal ] ]
]
