tests
testMacMetaShouldBeCmd
	OSPlatform current isMacOS ifFalse: [ ^ self skip ].
	self assert: $n meta equals: $n command.
	self assert: $n command equals: $n meta.
	self assert: $n meta shift equals: $n command shift.
	self assert: $n command shift equals: $n meta shift