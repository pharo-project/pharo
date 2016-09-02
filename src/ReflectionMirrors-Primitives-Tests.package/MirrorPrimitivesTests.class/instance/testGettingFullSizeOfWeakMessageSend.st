tests-fields accessing
testGettingFullSizeOfWeakMessageSend

	| actual arrayWithInstVars |
	arrayWithInstVars := WeakMessageSend new.
	
	actual := MirrorPrimitives sizeOf: arrayWithInstVars.
	
	self assert: actual equals: 1 + WeakMessageSend instSize "receiver of weak message send is stored as first array item"