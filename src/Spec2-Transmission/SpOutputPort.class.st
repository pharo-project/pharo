"
! Output ports
An output port defines origin actions (and the possible data associated to such action) to transmit to an destination (input) port.

It also defines the trasformations to apply to the output data before give them to the input port.

Finally, it can also define some operation to do after the transmission is completed. 

Transmission are ''attached'' to a presenter (each concrete output port will override *method:SpOutputPort#attachTransmission:*).
"
Class {
	#name : #SpOutputPort,
	#superclass : #SpPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #attaching }
SpOutputPort >> attachTransmission: aTransmission [
	"A transmission is no more than a definition of what to transmit to who. 
	 By overriding this method, concrete output ports can listen the event they need to and 
	 execute the transmission (sending ==#transmitTo:value:==).
	[[[language=Smalltalk
	self destinationPresenter whenSelectionChangedDo: [ :selection | 
		self 
			transmitWith: aTransmission 
			value: selection transmission ]
	]]]"

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
SpOutputPort >> transmitTo: aPort postTransmission: postValuable [

	^ SpTransmission new
		fromPort: self;
		toPort: aPort;
		postTransmission: postValuable;
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
