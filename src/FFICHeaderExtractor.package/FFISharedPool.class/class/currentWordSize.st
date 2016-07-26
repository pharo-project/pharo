accessing
currentWordSize
	"Answers the word size of the current platform. This could be either 32 or 64. Right now the way we obtain this is via FFIExternalType"
	^ FFIExternalType sizeOf: #int