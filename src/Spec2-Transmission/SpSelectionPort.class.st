"
! Selection Output Port (==SpSelectionPort==)
A """"selection output port"""" will handle the transmission when a presenter can be selected (e.g. lists).

The presenter exposing a selection output port need to define also the event ==#whenSelectionChangedDo:==.

Presenters exposing this port: *class:SpAbstractListPresenter* subclasses and *class:SpTreeTablePresenter*
"
Class {
	#name : #SpSelectionPort,
	#superclass : #SpOutputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpSelectionPort class >> portName [
	
	^ #selection
]

{ #category : #attaching }
SpSelectionPort >> attachTransmission: aTransmission [

	self destinationPresenter whenSelectionChangedDo: [ :selection | 
		self 
			transmitWith: aTransmission 
			value: selection transmission ]
]
