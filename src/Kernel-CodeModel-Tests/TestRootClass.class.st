Class {
	#name : 'TestRootClass',
	#superclass : 'Object',
	#classVars : [
		'SomeSharedVariable'
	],
	#pools : [
		'TestSharedPool'
	],
	#category : 'Kernel-CodeModel-Tests-Support',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Support'
}

{ #category : 'support' }
TestRootClass >> methodUsingSharedPoolVariable [
	^ One
]

{ #category : 'support' }
TestRootClass >> methodUsingSomeSharedVariable [
	^ SomeSharedVariable
]
