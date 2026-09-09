Extension { #name : 'ExceptionTest' }

{ #category : '*Debugging-Utils-Tests' }
ExceptionTest >> testCaptureIfSignalledWhenStepping [
	self deny: Exception captureIfSignalledWhenStepping.
	self assert: Error captureIfSignalledWhenStepping.
	self deny: (Error allSubclasses anySatisfy: [ :c| c  captureIfSignalledWhenStepping]).
	self assert: (UnhandledException withAllSubclasses select:[:c| c captureIfSignalledWhenStepping]) size equals: 1.
	self assert: (UnhandledException withAllSubclasses select:[:c| c captureIfSignalledWhenStepping]) first identicalTo: Halt
]

{ #category : '*Debugging-Utils-Tests' }
ExceptionTest >> testUnhandledExceptionShouldAllowToCatchHalt [

	| halt caughtException |
	halt := Halt new.
	self runWithNoHandlers: [
		[ halt signal ] on: UnhandledException do: [ :err | caughtException := err ]
	].
	self assert: caughtException equals: halt
]
