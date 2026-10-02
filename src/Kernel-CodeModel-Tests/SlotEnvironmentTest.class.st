Class {
	#name : 'SlotEnvironmentTest',
	#superclass : 'SlotSilentTest',
	#category : 'Kernel-CodeModel-Tests-ClassBuilder',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'ClassBuilder'
}

{ #category : 'tests' }
SlotEnvironmentTest >> testBasicEnvironment [
	| environment |
	environment := SystemEnvironment new.

	aClass := self make: [ :builder | builder environment: environment ].

	self assert: aClass environment identicalTo: environment.
	self assert: (environment includesKey: aClass name).
	self assert: (environment at: aClass name) identicalTo: aClass.

	self deny: (testingEnvironment includesKey: aClass name)
]
