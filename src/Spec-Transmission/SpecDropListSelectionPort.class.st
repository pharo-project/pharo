"
I'm a selection transmission specific of a drop-list.
I will handle the transmission when an item is selected on a drop list.
"
Class {
	#name : #SpecDropListSelectionPort,
	#superclass : #SpecOutputPort,
	#category : #'Spec-Transmission-Base'
}

{ #category : #accessing }
SpecDropListSelectionPort class >> portName [
	
	^ #selection
]

{ #category : #attaching }
SpecDropListSelectionPort >> attachTransmission: aTransmission [
	
	self destinationPresenter whenSelectionChangedDo: [ :selection | 
		self 
			transmitWith: aTransmission 
			value: selection selectedItem model ]
]
