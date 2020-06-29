"
! Text Input Port (==SpTextPort==)
A """"text input port"""" define an incomming transmission to modify the text of a text presenter (input fields or text areas).

The presenter exposing a text input port need to define also the method ==#text:==.

Presenters exposing this port: *class:SpAbstractTextPresenter* subclasses.
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
