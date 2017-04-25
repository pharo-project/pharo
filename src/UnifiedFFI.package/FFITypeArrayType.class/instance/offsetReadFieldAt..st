emitting code
offsetReadFieldAt: offsetVariableName
	"Return a string defining the accessor to an entity of the receiver type starting at the given byte offset. "
	self isPointer ifTrue: [ ^ super offsetReadFieldAt: offsetVariableName ].
	^ self offsetReadArrayAt: offsetVariableName