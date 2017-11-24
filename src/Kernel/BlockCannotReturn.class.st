"
This exception is thrown when a block tries to return but there is no home context.
"
Class {
	#name : #BlockCannotReturn,
	#superclass : #CannotReturn,
	#instVars : [
		'home'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #signalling }
BlockCannotReturn class >> result: anObject from: homeContext [

	^self new 
		result: anObject;
		home: homeContext;
		signal
]

{ #category : #accessing }
BlockCannotReturn >> home [
	^ home
]

{ #category : #accessing }
BlockCannotReturn >> home: anObject [
	home := anObject
]
