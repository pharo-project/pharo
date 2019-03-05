Class {
	#name : #TextPresenterPort,
	#superclass : #InputPresenterPort,
	#category : #'Spec-Core-Transmission'
}

{ #category : #'handling transmission' }
TextPresenterPort >> incomingTransmission: anObject from: outPort [

	self presenter text: anObject
]
