"
Tests related to special selector used bu the VM for some callback messages
"
Class {
	#name : #SpecialSelectorTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Special'
}

{ #category : #tests }
SpecialSelectorTest >> testMessageNotUnderstood [

	| object result |
	object := TestSpecialSelector new.
	result := object max: 42.
	self assert: result key equals: #doesNotUnderstand:.
	self assert: result value selector equals: #max:.
	self assert: result value arguments equals: #( 42 ).
	self assert: result value lookupClass equals: TestSpecialSelector
]
