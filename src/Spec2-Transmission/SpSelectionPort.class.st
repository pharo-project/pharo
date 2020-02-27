"
I'm a selection transmission. 
I will handle the transmission when a presenter can be selected (e.g. lists).
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
