"
! Label Input Port (==SpLabelPort==)
A """"label input port"""" define an incomming transmission to modify the label of alabel presenter.

The presenter exposing a text input port need to define also the method ==#label:==.

Presenters exposing this port: *class:SpLabelPresenter*.
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
