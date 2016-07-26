accessing
environ
	"Return the address of the array holding the environment variables"
	^ FFIExternalArray 
		fromPointer: (ExternalAddress loadSymbol: 'environ' from: LibC)
		type: String