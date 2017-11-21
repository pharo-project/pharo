"
ZnServerWriteErrorEvent signals an error during the writing of a response. These errors are considered mostly benign and the connection gets closed.

"
Class {
	#name : #ZnServerWriteErrorEvent,
	#superclass : #ZnServerErrorEvent,
	#category : #Zinc-HTTP-Logging
}

{ #category : #printing }
ZnServerWriteErrorEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Server Write Error ';  print: exception
]
