stream in
skip: anInteger
	"Skip a number of bytes.
	This is faster than #next: since it does not
	have to copy and return a new String or ByteArray.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!"

	self receiveData: anInteger.
	lastRead := (lastRead + anInteger) min: inNextToWrite - 1