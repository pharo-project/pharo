Class {
	#name : 'SycMockClassForHierarchicalSenderB',
	#superclass : 'SycMockClassForHierarchicalSenderA',
	#category : 'SystemCommands-MessageCommands-Tests',
	#package : 'SystemCommands-MessageCommands-Tests'
}

{ #category : 'mocking' }
SycMockClassForHierarchicalSenderB >> referredSelector [

	3 + 4
]

{ #category : 'mocking' }
SycMockClassForHierarchicalSenderB >> targetMethod [

	self referredSelector.
]
