"
I'm an input changed transmission. 
I will handle the transmission when an input presenter content changes.
"
Class {
	#name : #SpTextChangedPort,
	#superclass : #SpOutputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpTextChangedPort class >> portName [
	^ #changed
]

{ #category : #attaching }
SpTextChangedPort >> attachTransmission: aTransmission [
	self destinationPresenter whenTextChangedDo: [ :selection | self transmitWith: aTransmission value: selection text ]
]
