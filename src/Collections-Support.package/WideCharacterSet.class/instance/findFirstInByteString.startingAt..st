collection ops
findFirstInByteString: aByteString startingAt: startIndex
	"Double dispatching: since we know first argument is a ByteString, 
	we can use a ByteString primitive."
	
	^ ByteString
		findFirstInString: aByteString
		inSet: byteArrayMap
		startingAt: startIndex