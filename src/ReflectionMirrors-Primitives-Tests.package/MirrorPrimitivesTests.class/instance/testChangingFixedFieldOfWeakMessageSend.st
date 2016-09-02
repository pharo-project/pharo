tests-fields accessing
testChangingFixedFieldOfWeakMessageSend

	| arrayWithInstVars |
	"WeakMessageSend instVarNames #(#selector #shouldBeNil #arguments)"
	arrayWithInstVars := WeakMessageSend receiver: #receiver selector: #selector arguments: #(#args).
	
	MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 1 put: #newSelector.	
	self assert: arrayWithInstVars selector equals: #newSelector.
	
	MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 3 put: #newArgs.	
	self assert: arrayWithInstVars arguments equals: #newArgs.
	
	self
	 	should: [MirrorPrimitives fixedFieldOf: arrayWithInstVars at: 5 put: 100] 
		raise: PrimitiveFailed