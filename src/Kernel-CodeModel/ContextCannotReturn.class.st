"
This exception is thrown when a value is returned from a context, but the context is dead and the return can not be done.
"
Class {
	#name : 'ContextCannotReturn',
	#superclass : 'CannotReturn',
	#instVars : [
		'target'
	],
	#category : 'Kernel-CodeModel-Exceptions',
	#package : 'Kernel-CodeModel',
	#tag : 'Exceptions'
}

{ #category : 'signalling' }
ContextCannotReturn class >> result: anObject to: aContext [

	^self new
		result: anObject;
		target: aContext;
		signal
]

{ #category : 'accessing' }
ContextCannotReturn >> target [
	^ target
]

{ #category : 'accessing' }
ContextCannotReturn >> target: anObject [
	target := anObject
]
