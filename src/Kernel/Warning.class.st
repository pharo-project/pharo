"
A Warning is a Notification which by default should be brought to the attention of the user.
"
Class {
	#name : #Warning,
	#superclass : #Notification,
	#category : #'Kernel-Exceptions'
}

{ #category : #handling }
Warning >> defaultAction [
	"No one has handled this warning, but now give them a chance to decide how to debug it. 
	If none handles this either then open debugger (see UnhandedError-defaultAction).
	
	Note about naming here: 
	Even if warnings are not errors the absence of a handler for signaled warning is considered as an error by default although it can be resumed because warnings are resumable.
	Therefore #defaultAction for Warning raises an error, an UnhandledError"

	self raiseUnhandledError
]

{ #category : #handling }
Warning >> unhandledErrorAction [
	"No one has handled this warning, then I gave them a chance to decide how to debug it and I raise an UnhandledError. But it was not handled by anybody so that we are here (see UnhandedError-defaultAction).
	It is the final action an exception can do which is normally a debugger in interactive image"

	^UIManager default warningDefaultAction: self
]
