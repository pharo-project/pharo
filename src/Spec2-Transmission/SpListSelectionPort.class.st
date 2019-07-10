"
I'm a selection transmission. 
I will handle the transmission when a presenter can be selected (e.g. lists).
"
Class {
	#name : #SpListSelectionPort,
	#superclass : #SpOutputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpListSelectionPort class >> portName [
	
	^ #selection
]

{ #category : #attaching }
SpListSelectionPort >> attachTransmission: aTransmission [

	self destinationPresenter whenSelectionChangedDo: [ :selection | 
		self 
			transmitWith: aTransmission 
			value: selection selectedItem ]
]
