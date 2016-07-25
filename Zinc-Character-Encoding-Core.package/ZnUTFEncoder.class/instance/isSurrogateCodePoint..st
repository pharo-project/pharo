testing
isSurrogateCodePoint: codePoint
	"Surrogate Code Points should not be encoded or decoded because they are not Unicode scalar values"
	
	^ codePoint between: 16rD800 and: 16rDFFF