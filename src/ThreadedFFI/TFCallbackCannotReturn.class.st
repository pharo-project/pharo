Class {
	#name : #TFCallbackCannotReturn,
	#superclass : #Error,
	#instVars : [
		'callbackInvocation'
	],
	#category : #'ThreadedFFI-Callbacks'
}

{ #category : #accessing }
TFCallbackCannotReturn >> callback [
	^ self callbackInvocation callback
]

{ #category : #accessing }
TFCallbackCannotReturn >> callbackInvocation [
	^ callbackInvocation
]

{ #category : #accessing }
TFCallbackCannotReturn >> callbackInvocation: anObject [
	callbackInvocation := anObject
]
