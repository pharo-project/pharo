"
I define an incomming transmission to modify the label of alabel presenter.
"
Class {
	#name : #SpLabelPort,
	#superclass : #SpInputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpLabelPort class >> portName [

	^ #text
]

{ #category : #'handling transmission' }
SpLabelPort >> incomingTransmission: anObject from: outPort [

	self destinationPresenter label: anObject
]
