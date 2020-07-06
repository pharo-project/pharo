"
! Input ports
Input ports define the transmission destination points of a presenter. 
They handle an incoming transmissions and transmit them properly to the target presenter.

This transmission happens in *method:SpInputPort#incomingTransmission:from:*, that concrete implementations of input ports needs to define to populate the destination presenter.

See: *class:SpLabelPort*,*class:SpListItemsPort*, *class:SpModelPort*, *class:SpTextPort*, *class:SpActionPort*
"
Class {
	#name : #SpInputPort,
	#superclass : #SpPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #'handling transmission' }
SpInputPort >> incomingTransmission: anObject from: outPort [
	"When a transmission happens, the origin transmission has is triggers a value ==anObject== 
	 (already transformed if #transform: is specified) from the outPort *class:SpOutputPort*.
	 This method uses the incoming value to populate the destination presenter, according 
	 with the concrete port definition. 
	 Yes, this sounds abstract, you can check concrete implementations for a better 
	 understanding of it.
	[[[language=Smalltalk
	self destinationPresenter setModel: anObject
	]]]"

	self subclassResponsibility
]

{ #category : #testing }
SpInputPort >> isInput [ 

	^ true
]
