"
I handle a Session for a specific class, providing the API the class should implement.
Handlers will be triggered by the SessionManager.
"
Class {
	#name : #ClassSessionHandler,
	#superclass : #AbstractSessionHandler,
	#instVars : [
		'registeredClassName'
	],
	#category : #System-SessionManager
}

{ #category : #'instance creation' }
ClassSessionHandler class >> forClassNamed: aClassName [
	^ self new
		registeredClassName: aClassName;
		yourself
]

{ #category : #accessing }
ClassSessionHandler >> handledId [
	^ registeredClassName
]

{ #category : #handlers }
ClassSessionHandler >> registeredClass [
	^ Smalltalk globals at: registeredClassName asSymbol
]

{ #category : #accessing }
ClassSessionHandler >> registeredClassName: aClassName [ 
	registeredClassName := aClassName
]

{ #category : #handlers }
ClassSessionHandler >> shutdown: isImageQuitting [
	self registeredClass shutDown: isImageQuitting
]

{ #category : #handlers }
ClassSessionHandler >> startup: isImageStarting [
	self registeredClass startUp: isImageStarting
]
