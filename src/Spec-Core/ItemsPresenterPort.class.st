Class {
	#name : #ItemsPresenterPort,
	#superclass : #InputPresenterPort,
	#category : #'Spec-Core-Transmission'
}

{ #category : #'handling transmission' }
ItemsPresenterPort >> incomingTransmission: anObject from: outPort [

	self presenter items: anObject
]
