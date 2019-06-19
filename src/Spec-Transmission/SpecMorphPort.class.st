"
I define an incomming transmission to modify the morph value of a presenter.
I'm used on MorphPresenter.
"
Class {
	#name : #SpecMorphPort,
	#superclass : #SpecInputPort,
	#category : #'Spec-Transmission'
}

{ #category : #accessing }
SpecMorphPort class >> portName [

	^ #morph
]

{ #category : #'handling transmission' }
SpecMorphPort >> incomingTransmission: anObject from: aTransmission [

	self destinationPresenter morph: anObject
]
