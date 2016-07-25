*UnifiedFFI
unpackHandleFromArity: arity
	"This will 'unpack' a long (handle) from a certain arity. See #packToArity: for a better explanation."

	arity > 1 ifTrue: [ ^ self error: 'Use ExternalAddress instead!' ].
	^ self unsignedLongAt: 1
