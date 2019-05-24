"
I define an incomming transmission to modify the text of a text style presenter (input fields or text areas).
"
Class {
	#name : #SpecTextPort,
	#superclass : #SpecInputPort,
	#category : #'Spec-Transmission-Base'
}

{ #category : #accessing }
SpecTextPort class >> portName [

	^ #text
]

{ #category : #'handling transmission' }
SpecTextPort >> incomingTransmission: anObject from: outPort [

	self destinationPresenter text: anObject
]
