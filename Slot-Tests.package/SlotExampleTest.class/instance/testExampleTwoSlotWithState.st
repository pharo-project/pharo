tests
testExampleTwoSlotWithState
	" add two, remove one"
	aClass := self make: [ :builder |
		builder 
			slots: {#slot1 =>ExampleSlotWithState . #slot2 =>ExampleSlotWithState}
		].

	self assert: (aClass hasSlotNamed: #slot1).
	self assert: (aClass hasSlotNamed: #slot2).
	
	aClass := self make: [ :builder |
		builder 
			slots: {#slot2 =>ExampleSlotWithState}
		].
	self deny: (aClass hasSlotNamed: #slot1).
	self assert: (aClass hasSlotNamed: #slot2).