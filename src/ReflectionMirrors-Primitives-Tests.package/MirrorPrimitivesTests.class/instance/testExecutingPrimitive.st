tests-message performing
testExecutingPrimitive
	| actual |
	<expectedFailure> "it will be supported by VM at some point"
	
	actual := MirrorPrimitives withReceiver: 100 tryPrimitive: 1 withArguments: #(2).
	
	self assert: actual equals: 102