"
I am an abstract class defining the behavior of debugger that will log the error.
"
Class {
	#name : #LogDebugger,
	#superclass : #AbstractDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #testing }
LogDebugger class >> isAbstract [
	^ self = LogDebugger
]

{ #category : #'opening api' }
LogDebugger class >> messageToPrintFrom: aDebugSession [
	^ String
		streamContents: [ :s | 
			s
				<< 'ERROR. Here is the stack:';
				<< OSPlatform current lineEnding.
			aDebugSession interruptedContext shortDebugStackOn: s ]
]
