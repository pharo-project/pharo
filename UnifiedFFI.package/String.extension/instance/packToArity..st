*UnifiedFFI
packToArity: arity
	"WARNING: Here applies the same rules as in ByteArray>>#rollToArity:, check there!"
	arity = 1 ifTrue: [ ^ self ].
	self error: 'Use ExternalAddress instead!'