tests-fields accessing
testGettingFixedFieldOfWeakMessageSend

	| actual  arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	actual := MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 1.	
	self assert: actual equals: #selector. "receiver is stored as first array item"
	
	actual := MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 3.	
	self assert: actual first equals: #args.
	
	self should: [MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 5] raise: PrimitiveFailed