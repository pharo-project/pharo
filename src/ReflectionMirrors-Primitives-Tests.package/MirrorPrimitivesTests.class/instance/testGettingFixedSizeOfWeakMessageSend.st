tests-fields accessing
testGettingFixedSizeOfWeakMessageSend

	| actual arrayWithInstVars |
	arrayWithInstVars := WeakMessageSend new.
	
	actual := MirrorPrimitives fixedSizeOf: arrayWithInstVars.
	
	self assert: actual equals: WeakMessageSend instSize