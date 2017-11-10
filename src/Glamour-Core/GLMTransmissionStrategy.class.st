"
The classes from this hierarchy define strategies for what should happen after the value have been set to the destination port.
"
Class {
	#name : #GLMTransmissionStrategy,
	#superclass : #Object,
	#instVars : [
		'transmission'
	],
	#category : #Glamour-Core
}

{ #category : #'instance creation' }
GLMTransmissionStrategy class >> of: aTransmission [
	^ self new transmission: aTransmission; yourself
]

{ #category : #accessing }
GLMTransmissionStrategy >> addPresentation: aPresentation [
	"do nothing by default"
]

{ #category : #accessing }
GLMTransmissionStrategy >> addPresentations: aCollection [
	"do nothing by default"
]

{ #category : #accessing }
GLMTransmissionStrategy >> presentations [
	^ OrderedCollection new
]

{ #category : #accessing }
GLMTransmissionStrategy >> transmission [
	^ transmission
]

{ #category : #accessing }
GLMTransmissionStrategy >> transmission: anObject [
	transmission := anObject
]

{ #category : #transmitting }
GLMTransmissionStrategy >> transmitIn: aContext [
	"override this method to add the desired semantics"
	self subclassResponsibility
]
