Class {
	#name : 'SycMockClassForHierarchicalSenderB',
	#superclass : 'SycMockClassForHierarchicalSenderA',
	#category : 'SystemCommands-MessageCommands-Tests',
	#package : 'SystemCommands-MessageCommands-Tests'
}

{ #category : 'as yet unclassified' }
SycMockClassForHierarchicalSenderB >> referredSelector [

	3 + 4
]

{ #category : 'as yet unclassified' }
SycMockClassForHierarchicalSenderB >> targetMethod [

	self referredSelector.
]
