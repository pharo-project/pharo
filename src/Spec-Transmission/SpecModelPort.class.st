"
I define an incomming transmission to modify the model of a presenter.
"
Class {
	#name : #SpecModelPort,
	#superclass : #SpecInputPort,
	#category : #'Spec-Transmission-Base'
}

{ #category : #accessing }
SpecModelPort class >> portName [
	
	^ #model
]

{ #category : #'handling transmission' }
SpecModelPort >> incomingTransmission: anObject from: aTransmission [

	self destinationPresenter setModel: anObject
]
