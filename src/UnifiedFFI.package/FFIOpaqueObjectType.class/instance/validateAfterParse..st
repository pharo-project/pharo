testing
validateAfterParse: typeAndArityTuple
	"external objects have a natural arity of zero but they MUST be called with some arity, 
	 because they are actually external addresses (pointers). 
	 That means, you need to always declare external objects as this example:
	 [[[
		self ffiCall: #( FFIExternalObject *c_function ( FFIExternalObject *handle ) )  
	 ]]] 
	"
	self pointerArity >= 1 ifTrue: [ ^ self ].
	typeAndArityTuple second >= 1 ifTrue: [ ^ self ].
	
	FFIDereferencedOpaqueObjectError signal