stream in
upToAll: aStringOrByteArray
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	"Note: The 100k limit below is compatible with the previous version though arguably incorrect. If you need unbounded behavior either up the argument or provide nil in which case we'll read until we get it or run out of memory"

	^self upToAll: aStringOrByteArray limit: 100000