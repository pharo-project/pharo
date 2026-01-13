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

{ #category : 'configuring' }
CoDependencyPackageScopedSelectorTestReceiverStub >> beVariable [

    isVariable := true
]

{ #category : 'accessing' }
CoDependencyPackageScopedSelectorTestReceiverStub >> isVariable [

    ^ isVariable = true
]
