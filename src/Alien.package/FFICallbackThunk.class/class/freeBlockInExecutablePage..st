executable page management
freeBlockInExecutablePage: address
	ExecutablePages do:
		[:alienPage| | alienAddress |
		alienAddress := alienPage address.
		(address >= alienAddress
		 and: [alienAddress + alienPage dataSize >= address]) ifTrue:
			[alienPage unsignedByteAt: address - alienAddress + 1 put: 0.
			 ^self]]
			