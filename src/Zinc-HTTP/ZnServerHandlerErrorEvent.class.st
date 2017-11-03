"
ZnServerHandlerErrorEvent signals an error that occurred during handling by the delegate. This is considered a real error.
"
Class {
	#name : #ZnServerHandlerErrorEvent,
	#superclass : #ZnServerErrorEvent,
	#category : #Zinc-HTTP-Logging
}

{ #category : #printing }
ZnServerHandlerErrorEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Server Handler Error ';  print: exception
]
