"
I am an error produced when the callback cannot return correctly.
"
Class {
	#name : 'TFCallbackCannotReturn',
	#superclass : 'Error',
	#instVars : [
		'callbackInvocation'
	],
	#category : 'ThreadedFFI-Callbacks',
	#package : 'ThreadedFFI',
	#tag : 'Callbacks'
}

{ #category : 'accessing' }
TFCallbackCannotReturn >> callback [
	^ self callbackInvocation callback
]

{ #category : 'accessing' }
TFCallbackCannotReturn >> callbackInvocation [
	^ callbackInvocation
]

{ #category : 'accessing' }
TFCallbackCannotReturn >> callbackInvocation: anObject [
	callbackInvocation := anObject
]
