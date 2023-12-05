Class {
	#name : 'ShSharedPoolChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-ClassBuilder-Tests-Changes',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Changes'
}

{ #category : 'tests' }
ShSharedPoolChangeDetectorTest >> testChangeInSharedPoolsIsDetected [

	newBuilder sharedPools: {#TestSharedPool2}.

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShSharedPoolChangeDetectorTest >> testNoChangeInSharedPoolsDetected [

	newBuilder sharedPools: #(TestSharedPool).

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
