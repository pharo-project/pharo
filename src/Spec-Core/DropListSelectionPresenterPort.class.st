Class {
	#name : #DropListSelectionPresenterPort,
	#superclass : #OutputPresenterPort,
	#category : #'Spec-Core-Transmission'
}

{ #category : #accessing }
DropListSelectionPresenterPort class >> portName [
	
	^ #selection
]

{ #category : #attaching }
DropListSelectionPresenterPort >> attachTransmission: aTransmission [
	
	self presenter whenSelectionChangedDo: [ :selection | 
		self 
			transmitWith: aTransmission 
			value: selection selectedItem model ]
]
