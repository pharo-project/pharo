conversion
nextPutAll: aString toStream: aStream
	"Handle fast conversion if ByteString"
	
	| lastIndex nextIndex |
	aString class == ByteString ifFalse: [
		aString do: [:char | self nextPut: char toStream: aStream].
		^self].
	
	aStream isBinary ifTrue: [
		aStream basicNextPutAll: aString.
		^self].
	self nextPutByteString: aString toStream: aStream.
	^self
