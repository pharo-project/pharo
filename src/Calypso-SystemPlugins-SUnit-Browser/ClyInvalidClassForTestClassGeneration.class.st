"
Description
--------------------

I am an exception raised when we want to generate a test class from an invalid class. (For example if the class is a metaclass)
"
Class {
	#name : #ClyInvalidClassForTestClassGeneration,
	#superclass : #Error,
	#instVars : [
		'baseClass'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Exceptions'
}

{ #category : #signalling }
ClyInvalidClassForTestClassGeneration class >> signalFor: aClass [
	^ self new
		baseClass: aClass;
		signal
]

{ #category : #accessing }
ClyInvalidClassForTestClassGeneration >> baseClass [
	^ baseClass
]

{ #category : #accessing }
ClyInvalidClassForTestClassGeneration >> baseClass: anObject [
	baseClass := anObject
]

{ #category : #accessing }
ClyInvalidClassForTestClassGeneration >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"
	
	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #printing }
ClyInvalidClassForTestClassGeneration >> standardMessageText [	
	^ String streamContents: [ :stream |
		stream print: self baseClass.
		stream nextPutAll: ' is not a valid class to generate a test class' ]
]
