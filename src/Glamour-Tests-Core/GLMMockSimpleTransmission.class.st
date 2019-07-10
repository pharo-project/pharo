Class {
	#name : #GLMMockSimpleTransmission,
	#superclass : #GLMTransmission,
	#instVars : [
		'wasTransmited'
	],
	#category : #'Glamour-Tests-Core'
}

{ #category : #'instance creation' }
GLMMockSimpleTransmission class >> new [

	^super new initialize
]

{ #category : #initialization }
GLMMockSimpleTransmission >> initialize [
	super initialize.
	wasTransmited := false
]

{ #category : #tests }
GLMMockSimpleTransmission >> transmit [

	wasTransmited := true.
	super transmit.
]

{ #category : #tests }
GLMMockSimpleTransmission >> transmitIn: aContext [

	wasTransmited := true.
	super transmitIn: aContext
]

{ #category : #tests }
GLMMockSimpleTransmission >> wasTransmited [
	^wasTransmited
]
