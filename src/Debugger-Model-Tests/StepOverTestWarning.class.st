"
I am a warning used by StepOverTest. When nobody handles me, my default action raises an UnhandledError like any warning, but I do not open a debugger.
"
Class {
	#name : 'StepOverTestWarning',
	#superclass : 'Warning',
	#category : 'Debugger-Model-Tests-Exceptions',
	#package : 'Debugger-Model-Tests',
	#tag : 'Exceptions'
}

{ #category : 'handling' }
StepOverTestWarning >> unhandledErrorAction [
	"Answer instead of opening a debugger, so that a test stepping over an unhandled instance cannot open a debugger"

	^ nil
]
