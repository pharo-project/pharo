"
I'm a base input port.
I handle an incomming transmission.
"
Class {
	#name : #SpInputPort,
	#superclass : #SpPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #'handling transmission' }
SpInputPort >> incomingTransmission: anObject from: outPort [

	self subclassResponsibility
]

{ #category : #testing }
SpInputPort >> isInput [ 

	^ true
]
