Class {
	#name : #RootsPresenterPort,
	#superclass : #InputPresenterPort,
	#category : #'Spec-Core-Widgets-Tree'
}

{ #category : #'handling transmission' }
RootsPresenterPort >> incomingTransmission: anObject from: outPort [

	self presenter roots: anObject
]
