"
Debugger failures are exceptions created when the Oups infrastructure is configured to debug error-handling errors.

When a debugger encounters an error while opening, a `DebuggerFailure` exception is instantiated and signaled.
A debugger failure references:

- the debugger that failed to open the original error: it is used by debugger selection strategies to avoid opening recursively the same faulty debugger,
-  the original error with its interrupted context: if the debugger error is fixed, the debugger failure is resumed and the fixed debugger opens the original error.

"
Class {
	#name : #OupsDebuggerFailure,
	#superclass : #Exception,
	#instVars : [
		'failedDebugger',
		'innerException'
	],
	#category : #'Oups-exceptions'
}

{ #category : #'instance creation' }
OupsDebuggerFailure class >> of: aDebugger on: anException [

	<debuggerCompleteToSender>
	^ self new
		  debugger: aDebugger exception: anException;
		  yourself
]

{ #category : #initialization }
OupsDebuggerFailure >> debugger: aDebugger exception: anException [
	failedDebugger := aDebugger.
	innerException := anException
]

{ #category : #accessing }
OupsDebuggerFailure >> failedDebugger [
	^ failedDebugger
]

{ #category : #accessing }
OupsDebuggerFailure >> innerException [
	^ innerException
]

{ #category : #testing }
OupsDebuggerFailure >> isDebuggerFailure [
	^true
]

{ #category : #testing }
OupsDebuggerFailure >> isHandleableBy: aDebugger [
	^ aDebugger ~= failedDebugger
]

{ #category : #accessing }
OupsDebuggerFailure >> originException [
	^ innerException originException
]
