tests-fields accessing
testChangingIndexableFieldOfWeakMessageSend

	| arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	MirrorPrimitives indexableFieldOf: arrayWithInstVars at: 1 put: #newReceiver.	
	self assert: arrayWithInstVars receiver equals: #newReceiver.
		
	self
	 	should: [MirrorPrimitives indexableFieldOf: arrayWithInstVars at: 2 put: 100] 
		raise: SubscriptOutOfBounds 