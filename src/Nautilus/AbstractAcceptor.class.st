"
I am an abstract acceptor.
The goal of my children is to properly dispatch the behavior when text is accepted. Depending of the context, different actions could be triggered
"
Class {
	#name : #AbstractAcceptor,
	#superclass : #Object,
	#instVars : [
		'model'
	],
	#category : 'Nautilus-Acceptors'
}

{ #category : #'instance creation' }
AbstractAcceptor class >> model: model [

	^ self new
		model: model;
		yourself
]

{ #category : #protocol }
AbstractAcceptor >> accept: aText notifying: aController [

	self subclassResponsibility
]

{ #category : #accessing }
AbstractAcceptor >> model [

	^ model
]

{ #category : #accessing }
AbstractAcceptor >> model: anObject [
	
	model := anObject
]
