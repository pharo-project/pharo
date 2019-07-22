"
I define an incomming transmission to modify the text of a text style presenter (input fields or text areas).
"
Class {
	#name : #SpTextPort,
	#superclass : #SpInputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpTextPort class >> portName [

	^ #text
]

{ #category : #'handling transmission' }
SpTextPort >> incomingTransmission: anObject from: outPort [

	self destinationPresenter text: anObject
]
