"
I'm a selection transmission. 
I will handle the transmission when a presenter can be selected (e.g. lists).
"
Class {
	#name : #SpecListSelectionPort,
	#superclass : #SpecOutputPort,
	#category : #'Spec-Transmission-Base'
}

{ #category : #accessing }
SpecListSelectionPort class >> portName [
	
	^ #selection
]

{ #category : #attaching }
SpecListSelectionPort >> attachTransmission: aTransmission [

	self destinationPresenter whenSelectionChangedDo: [ :selection | 
		self 
			transmitWith: aTransmission 
			value: selection selectedItem ]
]
