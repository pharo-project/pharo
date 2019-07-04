"
I'm a backend for a Spec applications. 
Backends define different things from an application: 
- which UI backend will use (Morphic by default), 
- access to system resources
- access to standard dialogs usually residing in the operating system (but also simulated)
- ...
"
Class {
	#name : #SpApplicationBackend,
	#superclass : #Object,
	#instVars : [
		'adapterBindings'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #accessing }
SpApplicationBackend class >> backendName [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpApplicationBackend class >> findBackendNamed: aName [

	^ (self allSubclasses 
		detect: [ :each | each backendName = aName ])
		new	
]

{ #category : #accessing }
SpApplicationBackend >> adapterBindings [

	^ adapterBindings
]

{ #category : #private }
SpApplicationBackend >> adapterBindingsClass [

	self subclassResponsibility
]

{ #category : #initialization }
SpApplicationBackend >> initialize [

	super initialize.
	adapterBindings := self adapterBindingsClass new
]

{ #category : #accessing }
SpApplicationBackend >> name [

	^ self class backendName
]

{ #category : #'ui notifying' }
SpApplicationBackend >> notify: aSpecNotification [

	aSpecNotification type 
		notify: aSpecNotification
		on: self
]

{ #category : #'ui notifying' }
SpApplicationBackend >> notifyError: aSpecNotification [

	self subclassResponsibility
]

{ #category : #'ui notifying' }
SpApplicationBackend >> notifyInfo: aSpecNotification [

	self subclassResponsibility
]

{ #category : #'ui dialogs' }
SpApplicationBackend >> selectFileTitle: aString [

	self subclassResponsibility
]
