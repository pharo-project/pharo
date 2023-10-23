Class {
	#name : 'ShSharedPoolChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
}

{ #category : 'tests' }
ShSharedPoolChangeDetectorTest >> testChangeInSharedPoolsIsDetected [

	newBuilder sharedPools: 'TestSharedPool2'.

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShSharedPoolChangeDetectorTest >> testNoChangeInSharedPoolsDetected [

	newBuilder sharedPools: 'TestSharedPool'.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
