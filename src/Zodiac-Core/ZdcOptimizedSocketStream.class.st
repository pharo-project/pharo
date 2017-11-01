"
I am ZnOptimizedSocketStream.

I am a ZdcSimpleSocketStream.

I re-implement the critical operations dealing with bulk input and output 
more efficiently to work with buffer sized chunks, bypassing #next and #nextPut:
"
Class {
	#name : #ZdcOptimizedSocketStream,
	#superclass : #ZdcSimpleSocketStream,
	#category : 'Zodiac-Core'
}

{ #category : #'stream out' }
ZdcOptimizedSocketStream >> next: count putAll: collection startingAt: offset [
	"Write count bytes from collection starting at offset. Overwritten, optimized"
	
	| totalWritten |
	totalWritten := 0.
	[ | toWrite |
		toWrite := (count - totalWritten) min: writeBuffer availableForWriting.
		writeBuffer next: toWrite putAll: collection startingAt: offset + totalWritten.
		totalWritten := totalWritten + toWrite.
		totalWritten = count ] whileFalse: [ self flushWriteBuffer ]
]

{ #category : #'stream in' }
ZdcOptimizedSocketStream >> readInto: collection startingAt: offset count: requestedCount [
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read. Overwritten, optimized"
	
	| totalRead |
	totalRead := 0.
	[ | read |
		(read := readBuffer availableForReading min: (requestedCount - totalRead)) > 0
			ifTrue: [  
				collection 
					replaceFrom: offset + totalRead to: offset + totalRead + read - 1 
					with: readBuffer buffer startingAt: readBuffer contentsStart.
				readBuffer advanceReadPointer: read.
				totalRead := totalRead + read ].
		totalRead = requestedCount or: [ self isConnected not ] ] whileFalse: [ self fillReadBuffer ].   
	^ totalRead
]

{ #category : #'stream in' }
ZdcOptimizedSocketStream >> skip: count [
	"Skip over count bytes. Overwritten, optimized"
	
	| skipCount |
	skipCount := 0.
	[ | leftToSkip skipping |
		leftToSkip := count - skipCount.
		skipping := readBuffer availableForReading min: leftToSkip.
		readBuffer advanceReadPointer: skipping.
	     (skipCount := skipCount + skipping) = count ] whileFalse: [ self fillReadBuffer ]
]

{ #category : #'stream in' }
ZdcOptimizedSocketStream >> upTo: value [ 
	"Read bytes upto but not including value and return them as a ByteArray.
	If value is not found, return the entire contents of the stream. Overwritten, optimized"
	
	^ self collectionSpecies 
		streamContents: [ :writeStream |
			[ | position |
				position := 0.
				readBuffer isEmpty ifFalse: [ | count |
					position := readBuffer buffer indexOf: value startingAt: readBuffer contentsStart.
					count := position = 0 ifTrue: [ readBuffer availableForReading ] ifFalse: [ position - readBuffer contentsStart ].
					writeStream next: count putAll: readBuffer buffer startingAt: readBuffer contentsStart.
					readBuffer advanceReadPointer: count.
					position = 0 ifFalse: [ readBuffer advanceReadPointer: 1 ] ].   
				position ~= 0 or: [ self atEnd ] ] whileFalse: [ 
					readBuffer isEmpty ifTrue: [ self fillReadBuffer ] ] ]
]

{ #category : #'stream in' }
ZdcOptimizedSocketStream >> upToEnd [
	"Read bytes until the stream is atEnd and return them as a ByteArray. Overwritten, optimized"

	^ self collectionSpecies 
		streamContents: [ :writeStream |
			[	
				readBuffer isEmpty ifFalse: [
					writeStream 
						next: readBuffer availableForReading 
						putAll: readBuffer buffer 
						startingAt: readBuffer contentsStart.
					readBuffer advanceReadPointer: readBuffer availableForReading ].   
				self atEnd ] whileFalse: [ 
					readBuffer isEmpty 
						ifTrue: [ 
							[ self fillReadBuffer ] 
								on: ConnectionClosed 
								do: [ :exception | exception return ] ] ] ]
]
