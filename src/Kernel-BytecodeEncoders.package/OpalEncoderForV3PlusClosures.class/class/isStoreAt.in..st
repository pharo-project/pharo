instruction stream support
isStoreAt: pc in: method
	"Answer whether the bytecode at pc is a store or store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 142)
		and: [
			"96 103		storeAndPopReceiverVariableBytecode"
			"104 111	storeAndPopTemporaryVariableBytecode"
			byte <= 111 
			or: [
				"129		extendedStoreBytecode"
				byte >= 129
				and: [
					"130		extendedStoreAndPopBytecode"
					byte <= 130
					or: [(
							"132		doubleExtendedDoAnythingBytecode"
							byte = 132	 
							and: [ (method at: pc+1) >= 160 ])
					or: [
						"141		storeRemoteTempLongBytecode"
						byte = 141
					or: [
						"142		storeAndPopRemoteTempLongBytecode"
						byte = 142]]]]]]