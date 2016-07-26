private
atomicTypeNamed: aString
	^AtomicTypes at: aString ifAbsent:[nil]