"
I'm a base output port.
I handle output transmissions by attaching them into a presenter.
"
Class {
	#name : #SpecOutputPort,
	#superclass : #SpecPort,
	#category : #'Spec-Transmission-Base'
}

{ #category : #attaching }
SpecOutputPort >> attachTransmission: aTransmission [ 
	"override this to add specific transmissions"

	self subclassResponsibility
]

{ #category : #testing }
SpecOutputPort >> isOutput [

	^ true
]

{ #category : #private }
SpecOutputPort >> transmitWith: aTransmission value: transmitted [

	aTransmission applyPreTransmissionWith: transmitted.
	aTransmission toPort
		incomingTransmission: (aTransmission transformed: transmitted)
		from: self.
	aTransmission applyPostTransmissionWith: transmitted
]
