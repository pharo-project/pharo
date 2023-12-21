Class {
	#name : 'SycMockClassForHierarhicalSenderB',
	#superclass : 'SycMockClassForHierarhicalSenderA',
	#category : 'SystemCommands-MessageCommands-Tests',
	#package : 'SystemCommands-MessageCommands-Tests'
}

{ #category : 'as yet unclassified' }
SycMockClassForHierarhicalSenderB >> referredSelector [

	3 + 4
]

{ #category : 'as yet unclassified' }
SycMockClassForHierarhicalSenderB >> targetMethod [

	self referredSelector.
]
