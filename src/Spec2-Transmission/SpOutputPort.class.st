"
I'm a base output port.
I handle output transmissions by attaching them into a presenter.
"
Class {
	#name : #SpOutputPort,
	#superclass : #SpPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #attaching }
SpOutputPort >> attachTransmission: aTransmission [ 
	"override this to add specific transmissions"

	self subclassResponsibility
]

{ #category : #testing }
SpOutputPort >> isOutput [

	^ true
]

{ #category : #private }
SpOutputPort >> transmitWith: aTransmission value: transmitted [

	aTransmission applyPreTransmissionWith: transmitted.
	aTransmission toPort
		incomingTransmission: (aTransmission transformed: transmitted)
		from: self.
	aTransmission applyPostTransmissionWith: transmitted
]
