testing
supportsWriteBarrier
	"Answer whether the VM observes the per-object read-only flag and consequently
	 aborts writes to inst vars of, and fails primitives that attempt to write, to read-only objects."

	^(self parameterAt: 65)
		ifNil: [false]
		ifNotNil:
			[:param| "In older VMs this is a boolean reflecting MULTIPLE_BYTECODE_SETS"
			 param isInteger "In newer VMs it is a set of integer flags, bit 1 of which is IMMUTABILITY"
				ifTrue: [param anyMask: 2]
				ifFalse: [false]]