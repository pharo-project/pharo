private
pointerSize: anInteger
	| spec |
	pointerSize := anInteger.
	(((spec := compiledSpec at: 1) anyMask: FFIFlagPointer)
	 and: [(spec bitAnd: FFIStructSizeMask) ~= anInteger]) ifTrue:
		[compiledSpec := compiledSpec shallowCopy.
		 compiledSpec at: 1 put: (spec bitClear: FFIStructSizeMask) + anInteger]