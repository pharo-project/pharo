"
I define an input port to delegate incomming transmission to modify items of a list (or table) style presenters.
"
Class {
	#name : #SpListItemsPort,
	#superclass : #SpInputPort,
	#category : #'Spec2-Transmission-Base'
}

{ #category : #accessing }
SpListItemsPort class >> portName [

	^ #items
]

{ #category : #'handling transmission' }
SpListItemsPort >> incomingTransmission: anObject from: outPort [

	self destinationPresenter items: anObject
]
