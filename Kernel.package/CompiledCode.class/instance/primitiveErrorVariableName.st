printing
primitiveErrorVariableName
	"Answer the primitive error code temp name, or nil if none."
	self isPrimitive ifTrue:
		[self pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil