Class {
	#name : #SpMockMillerPresenter,
	#superclass : #SpMillerPresenter,
	#instVars : [
		'activationBlock'
	],
	#category : #'Spec2-Tests-Miller'
}

{ #category : #simulating }
SpMockMillerPresenter >> activate [
	
	activationBlock value: (SpMillerActivation on: 1)
]

{ #category : #initialization }
SpMockMillerPresenter >> initializeWidgets [
	
	"Nothing"
]

{ #category : #initialization }
SpMockMillerPresenter >> whenActivatedDo: aBlock [

	activationBlock := aBlock
]
