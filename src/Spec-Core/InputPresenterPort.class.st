"
I'm a base input port.
I handle an incomming transmission.
"
Class {
	#name : #InputPresenterPort,
	#superclass : #PresenterPort,
	#category : #'Spec-Core-Transmission'
}

{ #category : #'handling transmission' }
InputPresenterPort >> incomingTransmission: anObject from: outPort [

	self subclassResponsibility
]

{ #category : #testing }
InputPresenterPort >> isInput [ 

	^ true
]
