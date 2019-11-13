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
