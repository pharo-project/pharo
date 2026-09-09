Class {
	#name : 'SlotEnvironmentTest',
	#superclass : 'SlotSilentTest',
	#category : 'Slot-Tests-ClassBuilder',
	#package : 'Slot-Tests',
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
