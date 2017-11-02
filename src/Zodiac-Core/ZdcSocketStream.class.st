"
I am ZdcSocketStream.

I am a ZdcOptimizedSocketStream.

I further optimize my superclass' methods dealing with bulk input and output
to transparently bypass the internal buffers when this makes sense.
"
Class {
	#name : #ZdcSocketStream,
	#superclass : #ZdcOptimizedSocketStream,
	#category : 'Zodiac-Core'
}

{ #category : #'stream out' }
ZdcSocketStream >> next: count putAll: collection startingAt: offset [
	"Write count bytes from collection starting at offset. Overwritten, optimized"
	
	count <= (writeBuffer bufferSize / 8)  
		ifTrue: [
			count > writeBuffer availableForWriting ifTrue: [ self flushWriteBuffer ].  
			writeBuffer next: count putAll: collection startingAt: offset ]
		ifFalse: [
			self flushWriteBuffer.
			self flushBytes: collection startingAt: offset count: count ]
]

{ #category : #'stream in' }
ZdcSocketStream >> readInto: collection startingAt: offset count: requestedCount [
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read. Overwritten, optimized"
	
	| totalRead read |
	totalRead := 0.
	(read := readBuffer availableForReading min: requestedCount) > 0
		ifTrue: [  
			collection replaceFrom: offset to: offset + read - 1 with: readBuffer buffer startingAt: readBuffer contentsStart.
			readBuffer advanceReadPointer: read.
			(totalRead := totalRead + read) = requestedCount ifTrue: [ ^ totalRead ] ].  
	[
		read := self fillBytes: collection startingAt: offset + totalRead count: requestedCount - totalRead.
		(totalRead := totalRead + read) = requestedCount or: [ self isConnected not ] ] whileFalse: [ 
			self socketWaitForData ].
	^ totalRead
]
