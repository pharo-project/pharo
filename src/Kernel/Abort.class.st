"
Notify to abort a task
"
Class {
	#name : #Abort,
	#superclass : #Exception,
	#category : #'Kernel-Exceptions'
}

{ #category : #accessing }
Abort >> defaultAction [
	"No one has handled this error, but now give them a chance to decide how to debug it. If none handle this either then open debugger (see UnhandledError>>#defaultAction)"

	UnhandledError signalForException: self
]
