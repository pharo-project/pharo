"
I define an input port to delegate incomming transmission to modify items of a list (or table) style presenters.
"
Class {
	#name : #SpecListItemsPort,
	#superclass : #SpecInputPort,
	#category : #'Spec-Transmission-Base'
}

{ #category : #accessing }
SpecListItemsPort class >> portName [

	^ #items
]

{ #category : #'handling transmission' }
SpecListItemsPort >> incomingTransmission: anObject from: outPort [

	self destinationPresenter items: anObject
]
