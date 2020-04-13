"
A Warning is a Notification which by default should be brought to the attention of the user.
"
Class {
	#name : #Warning,
	#superclass : #Notification,
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptionDescription }
Warning >> unhandledErrorAction [
	"No one has handled this error, then I gave them a chance to decide how to debug it and I raise an UnhandledError. But it was not handled by anybody so that we are here (see UnhandedError-defaultAction).
	It is the final action an exception can do which is normally a debugger in interactive image"

	^UIManager default warningDefaultAction: self
]
