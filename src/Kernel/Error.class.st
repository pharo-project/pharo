"
>From the ANSI standard:
This protocol describes the behavior of instances of class Error. These are used to represent error conditions that prevent the normal continuation of processing. Actual error exceptions used by an application may be subclasses of this class.
As Error is explicitly specified  to be subclassable, conforming implementations must implement its behavior in a non-fragile manner.

Additional notes:
Error>defaultAction uses an explicit test for the presence of the Debugger class to decide whether or not it is in development mode.  In the future, TFEI hopes to enhance the semantics of #defaultAction to improve support for pluggable default handlers.
"
Class {
	#name : #Error,
	#superclass : #Exception,
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptiondescription }
Error >> defaultAction [
	"No one has handled this error, but now give them a chance to decide how to debug it.  If none handle this either then open debugger (see UnhandedError-defaultAction)"

	UnhandledError signalForException: self
]

{ #category : #private }
Error >> isResumable [
	"Determine whether an exception is resumable."

	^ false
]
