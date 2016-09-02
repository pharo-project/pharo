tests-message performing
testPerformingMessage
	| actual |
	
	actual := MirrorPrimitives withReceiver: 0@0 perform: #distanceTo: withArguments: {0@2}.
	
	self assert: actual equals: 2