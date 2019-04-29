"
I'm a base output port.
I handle output transmissions by attaching them into a presenter.
"
Class {
	#name : #OutputPresenterPort,
	#superclass : #PresenterPort,
	#category : #'Spec-Core-Transmission'
}

{ #category : #attaching }
OutputPresenterPort >> attachTransmission: aTransmission [ 
	"override this to add specific transmissions"

	self subclassResponsibility
]

{ #category : #testing }
OutputPresenterPort >> isOutput [

	^ true
]

{ #category : #private }
OutputPresenterPort >> transmitWith: aTransmission value: transmitted [

	aTransmission applyPreTransmissionWith: transmitted.
	aTransmission toPort
		incomingTransmission: (aTransmission transformed: transmitted)
		from: self.
	aTransmission applyPostTransmissionWith: transmitted
]
