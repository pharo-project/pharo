tests-fields accessing
testGettingGeneralFieldOfWeakMessageSend

	| actual  arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	actual := MirrorPrimitives fieldOf: arrayWithInstVars at: 1.	
	self assert: actual equals: #receiver. "receiver is stored as first array item"
	
	actual := MirrorPrimitives fieldOf: arrayWithInstVars at: 2.	
	self assert: actual equals: #selector.
	
	actual := MirrorPrimitives fieldOf: arrayWithInstVars at: 4.	
	self assert: actual first equals: #args.
	
	self should: [MirrorPrimitives fieldOf: arrayWithInstVars at: 5] raise: PrimitiveFailed