tests-fields accessing
testGettingIndexibleSizeOfWeakMessageSend

	| actual arrayWithInstVars |
	arrayWithInstVars := WeakMessageSend new.
	
	actual := MirrorPrimitives indexableSizeOf: arrayWithInstVars.
	
	self assert: actual equals: 1 "receiver of weak message send is stored as first array item"