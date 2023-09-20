Class {
	#name : 'ShSharedPoolChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
}

{ #category : 'tests' }
ShSharedPoolChangeDetectorTest >> testChangeInSharedPoolsIsDetected [

	newBuilder sharedPools: 'TestSharedPool'.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]

{ #category : 'tests' }
ShSharedPoolChangeDetectorTest >> testNoChangeInSharedPoolsDetected [

	newBuilder sharedPools: 'TestSharedPool'.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
