Class {
	#name : #RootsPresenterPort,
	#superclass : #SpecInputPort,
	#category : #'Spec-Transmission'
}

{ #category : #'handling transmission' }
RootsPresenterPort >> incomingTransmission: anObject from: outPort [

	self presenter roots: anObject
]
