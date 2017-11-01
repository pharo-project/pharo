Class {
	#name : #SlotEnvironmentTest,
	#superclass : #SlotSilentTest,
	#category : 'Slot-Tests'
}

{ #category : #tests }
SlotEnvironmentTest >> testBasicEnvironment [
	| environment |
	environment := SystemDictionary new.
	
	aClass := self make: [ :builder | 
		builder environment: environment ].

	self assert: (aClass environment == environment).
	self assert: (environment includesKey: aClass name).	
	self assert: (environment at: aClass name) == aClass.
	
	self deny: (Smalltalk globals includesKey: aClass name).
]
