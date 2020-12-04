"
! Action Input Port (==SpActionPort==)
An """"action input port"""" s a generic action to handle the result of a transmission: Instead transmit to a destination presenter, it will execute an action block. 

[[[language=Smalltalk
aPresenter 
	transmitDo: [ :aValue | ... ]
	transform: #asString
]]]

""""Important:"""" This port is not meant to be used in the same way other input ports, instead, you will use the ==#transmitDo:== mechanism.
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
