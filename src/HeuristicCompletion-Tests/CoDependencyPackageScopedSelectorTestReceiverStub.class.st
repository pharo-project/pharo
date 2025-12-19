Class {
	#name : 'CoDependencyPackageScopedSelectorTestReceiverStub',
	#superclass : 'Object',
	#instVars : [
		'isVariable'
	],
	#category : 'HeuristicCompletion-Tests-Mocks',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Mocks'
}

{ #category : 'accessing' }
CoDependencyPackageScopedSelectorTestReceiverStub class >> variable [

    ^ self new
          beVariable;
          yourself
]

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestReceiverStub >> beVariable [

    isVariable := true
]

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestReceiverStub >> isVariable [

    ^ isVariable = true
]
