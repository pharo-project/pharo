Class {
	#name : #SpecMockMillerPresenter,
	#superclass : #SpecMillerPresenter,
	#instVars : [
		'activationBlock'
	],
	#category : #'Spec-Tests-Miller'
}

{ #category : #simulating }
SpecMockMillerPresenter >> activate [
	
	activationBlock value: (SpecMillerActivation on: 1)
]

{ #category : #initialization }
SpecMockMillerPresenter >> initializeWidgets [
	
	"Nothing"
]

{ #category : #initialization }
SpecMockMillerPresenter >> whenActivatedDo: aBlock [

	activationBlock := aBlock
]
