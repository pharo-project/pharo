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

{ #category : #transmitting }
SpOutputPort >> transmitDo: aBlock [
	
	^ SpTransmission new
		fromPort: self;
		do: aBlock;
		apply
]

{ #category : #transmitting }
SpOutputPort >> transmitDo: aBlock transform: transformBlock [
	
	^ SpTransmission new
		fromPort: self;
		do: aBlock;
		transform: transformBlock;
		apply
]

{ #category : #transmitting }
SpOutputPort >> transmitTo: aPort [

	^ SpTransmission new
		fromPort: self;
		toPort: aPort;
		apply
]

{ #category : #transmitting }
SpOutputPort >> transmitTo: aPort transform: aValuable [

	^ SpTransmission new
		fromPort: self;
		toPort: aPort;
		transform: aValuable;
		apply
]

{ #category : #transmitting }
SpOutputPort >> transmitTo: aPort transform: aValuable postTransmission: postValuable [

	^ SpTransmission new
		fromPort: self;
		toPort: aPort;
		transform: aValuable;
		postTransmission: postValuable;
		apply
]

{ #category : #private }
SpOutputPort >> transmitWith: aTransmission value: transmitted [

	aTransmission applyPreTransmissionWith: transmitted.
	aTransmission toPort
		incomingTransmission: (aTransmission transformed: transmitted)
		from: self.
	aTransmission applyPostTransmissionWith: transmitted
]
