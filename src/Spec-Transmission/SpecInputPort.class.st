"
I'm a base input port.
I handle an incomming transmission.
"
Class {
	#name : #SpecInputPort,
	#superclass : #SpecPort,
	#category : #'Spec-Transmission-Base'
}

{ #category : #'handling transmission' }
SpecInputPort >> incomingTransmission: anObject from: outPort [

	self subclassResponsibility
]

{ #category : #testing }
SpecInputPort >> isInput [ 

	^ true
]
