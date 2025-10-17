"
I am a process variable that defines if the process must stop during the fast step through
"
Class {
	#name : #DebuggerSteppingState,
	#superclass : #ProcessLocalVariable,
	#category : #'Debugger-Model-Core'
}

{ #category : #accessing }
DebuggerSteppingState >> default [

	^ false
]
