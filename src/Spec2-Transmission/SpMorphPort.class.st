"
I define an incomming transmission to modify the morph value of a presenter.
I'm used on MorphPresenter.
"
Class {
	#name : #SpMorphPort,
	#superclass : #SpInputPort,
	#category : #'Spec2-Transmission'
}

{ #category : #accessing }
SpMorphPort class >> portName [

	^ #morph
]

{ #category : #'handling transmission' }
SpMorphPort >> incomingTransmission: anObject from: aTransmission [

	self destinationPresenter morph: anObject
]
