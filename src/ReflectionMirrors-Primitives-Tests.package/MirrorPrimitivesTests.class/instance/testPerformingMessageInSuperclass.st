tests-message performing
testPerformingMessageInSuperclass
	| actual |
	
	actual := MirrorPrimitives withReceiver: 0@0 perform: #printString withArguments: #() inSuperclass: Object.
	
	self assert: actual equals: '(0@0)'