Class {
	#name : #SpRootsPort,
	#superclass : #SpInputPort,
	#category : #'Spec2-Transmission'
}

{ #category : #'handling transmission' }
SpRootsPort >> incomingTransmission: anObject from: outPort [

	self presenter roots: anObject
]
