"
! List Items Input Port (==SpListItemsPort==)
A """"list items input port"""" define an incomming transmission to modify items of a list, tree or table presenter.

The presenter exposing a list items input port need to define also the method ==#items:==.

Presenters exposing this port: *class:SpAbstractListPresenter* subclasses and: *class:SpDropListPresenter*.
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
