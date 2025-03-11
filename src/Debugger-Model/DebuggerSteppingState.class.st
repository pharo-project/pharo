"
I am a process variable to hold if we are stepping through or not
"
Class {
	#name : 'DebuggerSteppingState',
	#superclass : 'ProcessLocalVariable',
	#category : 'Debugger-Model-Core',
	#package : 'Debugger-Model',
	#tag : 'Core'
}

{ #category : 'accessing' }
DebuggerSteppingState >> default [

	^ false
]
