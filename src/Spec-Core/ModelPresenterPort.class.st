Class {
	#name : #ModelPresenterPort,
	#superclass : #InputPresenterPort,
	#category : #'Spec-Core-Transmission'
}

{ #category : #accessing }
ModelPresenterPort class >> portName [
	
	^ #model
]

{ #category : #'handling transmission' }
ModelPresenterPort >> incomingTransmission: anObject from: aTransmission [

	self presenter setModel: anObject
]
