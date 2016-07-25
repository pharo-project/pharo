accessing
atomicType
	^(self headerWord bitAnd: FFIAtomicTypeMask) >> FFIAtomicTypeShift