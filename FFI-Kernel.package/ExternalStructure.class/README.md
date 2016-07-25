This class provides an abstract base for all structures that can be used by external functions. ExternalStructures have two possible handle types:
	- ExternalAddress
		If the handle is an external address then the object described does not reside in the Smalltalk object memory.
	- ByteArray
		If the handle is a byte array then the object described resides in Smalltalk memory.
Useful methods should be implemented by subclasses of ExternalStructure using the common ByteArray/ExternalAddress platform dependent access protocol which will transparently access the correct memory location.