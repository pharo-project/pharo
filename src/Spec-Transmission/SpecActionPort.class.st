"
An action name is a generic action to handle the result of a transmission.
It will execute a block when transmission arrives.

Example: 

aPresenter 
	transmitDo: [ :aValue | ... ]
	transform: #asString
"
Class {
	#name : #SpecActionPort,
	#superclass : #SpecInputPort,
	#instVars : [
		'actionBlock'
	],
	#category : #'Spec-Transmission-Base'
}

{ #category : #'instance creation' }
SpecActionPort class >> newAction: aBlock [

	^ self new
		action: aBlock;
		yourself
]

{ #category : #accessing }
SpecActionPort class >> portName [
	"An action block does not has a name"
	
	^ nil
]

{ #category : #accessing }
SpecActionPort >> action [ 

	^ actionBlock
]

{ #category : #accessing }
SpecActionPort >> action: aBlock [

	actionBlock := aBlock
]

{ #category : #'handling transmission' }
SpecActionPort >> incomingTransmission: anObject from: outPort [

	self action 
		cull: anObject
		cull: outPort 
]
