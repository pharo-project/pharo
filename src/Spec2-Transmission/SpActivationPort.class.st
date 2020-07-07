"
! Activation Output Port (==SpActivationPort==)
A """"selection output port"""" will handle the transmission when a presenter can be activated (e.g. lists).

The presenter exposing a selection output port need to define also the event ==#whenActivatedDo:==.

Presenters exposing this port: *class:SpAbstractListPresenter* subclasses and *class:SpTreeTablePresenter*
"
Class {
	#name : #SpActivationPort,
	#superclass : #SpOutputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpActivationPort class >> portName [

	^ 'activation'
]

{ #category : #attaching }
SpActivationPort >> attachTransmission: aTransmission [

	self destinationPresenter whenActivatedDo: [ :selection | 
		self 
			transmitWith: aTransmission 
			value: selection ]
]
