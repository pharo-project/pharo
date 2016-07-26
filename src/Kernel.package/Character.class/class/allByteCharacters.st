instance creation
allByteCharacters
	"Answer all the characters that can be encoded in a byte"
	^ (0 to: 255) collect: [:v | Character value: v] as: String
