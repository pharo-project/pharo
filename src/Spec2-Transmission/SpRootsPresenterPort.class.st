Class {
	#name : #SpRootsPresenterPort,
	#superclass : #SpInputPort,
	#category : #'Spec2-Transmission'
}

{ #category : #'handling transmission' }
SpRootsPresenterPort >> incomingTransmission: anObject from: outPort [

	self presenter roots: anObject
]
