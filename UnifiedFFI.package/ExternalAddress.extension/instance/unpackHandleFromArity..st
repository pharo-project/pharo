*UnifiedFFI-private
unpackHandleFromArity: arity
	^ (self unpackFromArity: arity) unsignedLongAt: 1
	