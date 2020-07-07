"
! Model Input Port (==SpModelPort==)
A """"model input port"""" define an incomming transmission to modify the model of a presenter.

The presenter exposing a model input port need to define also the method ==#setModel:==.

Presenters exposing this port: *class:SpPresenterSelectorPresenter* and *class:SpPresenterWithModel*
"
Class {
	#name : #SpModelPort,
	#superclass : #SpInputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpModelPort class >> portName [
	
	^ #model
]

{ #category : #'handling transmission' }
SpModelPort >> incomingTransmission: anObject from: aTransmission [
	"sets model to destination presenter"

	self destinationPresenter setModel: anObject
]
