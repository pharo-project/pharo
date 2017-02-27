private
defineFieldAccessorsFor: fieldName type: type
	"Define read/write accessors for the given field. 
	 This method is equivallent to his parent, but it changes a couple of things: 
	 - type is FFIExternalType and not ExternalType
	 - it generates to another protocol, not accessing"
	| code |
	
	(type isVoid and: [ type isPointer not ]) ifTrue:[ ^ self ].
	
	code := fieldName,'
	"This method was automatically generated"
	', (type offsetReadFieldAt: (self offsetVariableNameFor: fieldName)).
	self 
		compile: code 
		classified: self generatedFieldProtocolName.
	
	code := fieldName,': anObject
	"This method was automatically generated"
	', (type offsetWriteFieldAt: (self offsetVariableNameFor: fieldName) with:'anObject').
	self 
		compile: code 
		classified: self generatedFieldProtocolName.