Class {
	#name : 'SycMockClassForHierarchicalSenderA',
	#superclass : 'Object',
	#category : 'SystemCommands-MessageCommands-Tests',
	#package : 'SystemCommands-MessageCommands-Tests'
}

{ #category : 'mocking' }
SycMockClassForHierarchicalSenderA >> methodReferToSelector [

	self referredSelector.
]

{ #category : 'mocking' }
SycMockClassForHierarchicalSenderA >> methodWithoutReference [

	4 + 6
]
