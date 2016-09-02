tests-message performing
testExecutingMethod
	| actual |
	
	actual := MirrorPrimitives withReceiver: 0@0 andArguments: {0@2} execute: (Point >> #distanceTo:).
	
	self assert: actual equals: 2