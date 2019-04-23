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
