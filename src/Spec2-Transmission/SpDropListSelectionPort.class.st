"
! Drop List Selection Output Port (==SpDropListSelectionPort==)
A """"drop list selection output port"""" is the same as *class:SpSelectionPort*, but to be used exclusively by *class:SpDropListPresenter*.

The presenter exposing a selection output port need to define also the event ==#whenSelectionChangedDo:==.

Presenters exposing this port: *class:SpDropListPresenter*

@@NOTE Maybe this needs to be merged with SpSelectionPort?
"
Class {
	#name : #SpDropListSelectionPort,
	#superclass : #SpOutputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpDropListSelectionPort class >> portName [
	
	^ #selection
]

{ #category : #attaching }
SpDropListSelectionPort >> attachTransmission: aTransmission [
	
	self destinationPresenter whenSelectionChangedDo: [ :selection | 
		self 
			transmitWith: aTransmission 
			value: (selection selectedItem ifNotNil:[ :item | item model ]) ]
]
