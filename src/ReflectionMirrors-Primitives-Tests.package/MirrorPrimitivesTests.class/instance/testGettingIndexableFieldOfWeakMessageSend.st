tests-fields accessing
testGettingIndexableFieldOfWeakMessageSend

	| actual  arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	actual := MirrorPrimitives indexableFieldOf: arrayWithInstVars at: 1.	
	self assert: actual equals: #receiver. "receiver is stored as first array item"
	
	self should: [MirrorPrimitives indexableFieldOf: arrayWithInstVars at: 2] raise: SubscriptOutOfBounds