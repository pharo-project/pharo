"
GLMPort represents the abstract port. Any port has a name. Subclasses can provide further semantics to a port.
"
Class {
	#name : #GLMPort,
	#superclass : #GLMLoggedObject,
	#instVars : [
		'name'
	],
	#category : 'Glamour-Core'
}

{ #category : #copying }
GLMPort >> copyAccordingToPaneMapping: newPanesMapping inNewBrowser: anotherBrowser [
	^ self subclassResponsibility
]

{ #category : #testing }
GLMPort >> hasPane [
	^ false
]

{ #category : #testing }
GLMPort >> isEntityPort [
	^ self name = #entity
]

{ #category : #testing }
GLMPort >> isSelectionPort [
	^ self name = #selection
]

{ #category : #accessing }
GLMPort >> name [ 
	 
	^name
]

{ #category : #accessing }
GLMPort >> name: aSymbol [ 
	 
	name := aSymbol
]

{ #category : #validation }
GLMPort >> notingPresentationChangeDo: aBlock [
	aBlock value
]

{ #category : #printing }
GLMPort >> printOn: aStream [ 
	super printOn: aStream.
	aStream 
		nextPut: Character space;
		nextPutAll: self identityHash printString;
		nextPutAll: ' (name='; 
		nextPutAll: self name printString; 
		nextPutAll: ' value='; 
		nextPutAll: self value printString; 
		nextPut: $)
]

{ #category : #accessing }
GLMPort >> receive: aTransmission [ 
	self receive: aTransmission in: GLMTransmissionContext new
]

{ #category : #accessing }
GLMPort >> receive: aTransmission in: aContext [ 
	self subclassResponsibility
]

{ #category : #accessing }
GLMPort >> silentValue: anObject [
	"This is meant to be used internally"
	self subclassResponsibility
]

{ #category : #accessing }
GLMPort >> transientValue: anObject [ 
	| previousValue |
	previousValue := self value. 
	self value: anObject.
	self silentValue: previousValue
]

{ #category : #validation }
GLMPort >> validate: anObject [ 

	^true
]

{ #category : #accessing }
GLMPort >> value [ 
	 
	self subclassResponsibility
]

{ #category : #accessing }
GLMPort >> value: anObject [ 
	self value: anObject in: GLMTransmissionContext new
]

{ #category : #accessing }
GLMPort >> value: anObject in: aContext [
	self subclassResponsibility
]
