"
ZnServerReadErrorEvent signals an error during the reading of a request. These errors are considered mostly benign and the connection gets closed.

"
Class {
	#name : #ZnServerReadErrorEvent,
	#superclass : #ZnServerErrorEvent,
	#category : #Zinc-HTTP-Logging
}

{ #category : #printing }
ZnServerReadErrorEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Server Read Error ';  print: exception
]
