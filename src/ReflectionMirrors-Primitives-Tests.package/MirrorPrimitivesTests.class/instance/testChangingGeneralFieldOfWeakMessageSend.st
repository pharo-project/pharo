tests-fields accessing
testChangingGeneralFieldOfWeakMessageSend

	| arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	MirrorPrimitives fieldOf: arrayWithInstVars at: 1 put: #newReceiver.	
	self assert: arrayWithInstVars receiver equals: #newReceiver.

	MirrorPrimitives fieldOf: arrayWithInstVars at: 2 put: #newSelector.	
	self assert: arrayWithInstVars selector equals: #newSelector.
	
	MirrorPrimitives fieldOf: arrayWithInstVars at: 4 put: #newArgs.	
	self assert: arrayWithInstVars arguments equals: #newArgs.
	
	self
	 	should: [MirrorPrimitives fieldOf: arrayWithInstVars at: 5 put: 100] 
		raise: PrimitiveFailed