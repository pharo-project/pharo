converting
encodedByteCountFor: character
	"Return how many bytes are needed to encode character"
	
	^ self encodedByteCountForCodePoint: character codePoint