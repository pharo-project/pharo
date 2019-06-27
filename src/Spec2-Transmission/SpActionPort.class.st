"
An action name is a generic action to handle the result of a transmission.
It will execute a block when transmission arrives.

Example: 

aPresenter 
	transmitDo: [ :aValue | ... ]
	transform: #asString
"
Class {
	#name : #SpActionPort,
	#superclass : #SpInputPort,
	#instVars : [
		'actionBlock'
	],
	#category : #'Spec2-Transmission-Base'
}

{ #category : #'instance creation' }
SpActionPort class >> newAction: aBlock [

	^ self new
		action: aBlock;
		yourself
]

{ #category : #accessing }
SpActionPort class >> portName [
	"An action block does not has a name"
	
	^ nil
]

{ #category : #accessing }
SpActionPort >> action [ 

	^ actionBlock
]

{ #category : #accessing }
SpActionPort >> action: aBlock [

	actionBlock := aBlock
]

{ #category : #'handling transmission' }
SpActionPort >> incomingTransmission: anObject from: outPort [

	self action 
		cull: anObject
		cull: outPort 
]
