writing
nextWordsPut: aWordObject

	| byteSize |
	byteSize := aWordObject basicSize * 4.

	"Ensure we are at bigger than the words added, with size next power-of-two"
	byteSize > buffer size ifTrue: [ self sizeBuffer: 1 << (byteSize highBit)].
	
	"BitBlt needs word-aligned access of object. Flushing the buffer is a very good idea because after the position will be zero, which is word aligned.  Word objects always have a full number of words of data to write. (otherwise they'd be variableByte objects or something :P) So as long as the size you write per instance also takes N words, the position will always be aligned (except when writing the first object of a cluster) . After that, we flush when an object larger than current buffer is encountered (statistically rare), or if the buffer is full (which we do anyways).
	Finally, we also need enough free space in the buffer"
	((position bitAnd: 3) = 0 and: [byteSize < self bufferFreeSize])
      	ifFalse: [self flushBuffer].

	self copyWordObjectToBuffer: aWordObject.
	position := position +  byteSize.