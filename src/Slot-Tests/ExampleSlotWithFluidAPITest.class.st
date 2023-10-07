Class {
	#name : 'ExampleSlotWithFluidAPITest',
	#superclass : 'SlotSilentTest',
	#category : 'Slot-Tests-Examples',
	#package : 'Slot-Tests',
	#tag : 'Examples'
}

{ #category : 'tests' }
ExampleSlotWithFluidAPITest >> testExampleSlotWithFluidAPI [

	"Shows that we can use cascade (fluid api) to set meta data on slots. As the #=> has precedense, the cascade
	is sent to the slot instance"

	| slot |
	slot := #slot => ExampleSlotWithFluidAPI
		        value1: #testvalue1;
		        value2: #testvalue2.
	aClass := self make: [ :builder | builder slots: { slot } ].
	self assert: (aClass hasSlotNamed: #slot).

	self assert: slot value1 equals: #testvalue1.
	self assert: slot value2 equals: #testvalue2.
	self
		assert: slot printString
		equals: '#slot => ExampleSlotWithFluidAPI value1: #testvalue1; value2: #testvalue2'
]
