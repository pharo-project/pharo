"
A composited output port that allow presenters to trigger output transmissions to several subpresenters (or itself)
"
Class {
	#name : #SpCompositeOutputPort,
	#superclass : #SpOutputPort,
	#instVars : [
		'ports'
	],
	#category : #'Spec2-Transmission-Base'
}

{ #category : #adding }
SpCompositeOutputPort >> add: aPort [ 

	ports := ports copyWith: aPort
]

{ #category : #attaching }
SpCompositeOutputPort >> attachTransmission: aTransmission [

	ports do: [ :each | 
		each attachTransmission: aTransmission ]
]

{ #category : #initialization }
SpCompositeOutputPort >> initialize [

	super initialize.
	ports := #()
]
