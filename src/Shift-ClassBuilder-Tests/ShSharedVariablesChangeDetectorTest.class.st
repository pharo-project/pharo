Class {
	#name : 'ShSharedVariablesChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-ClassBuilder-Tests-Changes',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Changes'
}

{ #category : 'tests' }
ShSharedVariablesChangeDetectorTest >> testChangeInSharedVariablesIsDetected [

	newBuilder sharedVariables: { #Var1. #Var2 }.

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShSharedVariablesChangeDetectorTest >> testNoChangeInSharedVariablesDetected [

	newBuilder sharedVariables: { #Var1 }.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
