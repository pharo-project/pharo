"
I'm a selection transmission specific of a drop-list.
I will handle the transmission when an item is selected on a drop list.
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
			value: selection selectedItem model ]
]
