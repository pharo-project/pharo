testing
supportsMultipleBytecodeSets
	"Answer whether the VM supports multiple bytecodeSets."

	^(self parameterAt: 65)
		ifNil: [false]
		ifNotNil:
			[:param| "In older VMs this is a boolean reflecting MULTIPLE_BYTECODE_SETS"
			 param isInteger "In newer VMs it is a set of integer flags, bit 0 of which is MULTIPLE_BYTECODE_SETS"
				ifTrue: [param anyMask: 1]
				ifFalse: [param]]