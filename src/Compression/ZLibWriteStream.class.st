"
I use the zlib implementation of the deflate algorithm to compress a ByteArray. According to Wikipedia, ""this algorithm provides good compression on a wide variety of data with minimal use of system resources."" See http://en.wikipedia.org/wiki/DEFLATE for details on the deflate algorithm.

This is how you get a compressed ByteArray from your input:
zipStream := ZLibWriteStream on: (ByteArray new).
zipStream 
	nextPutAll: myByteArray;
	close.
compressed := zipStream encodedStream contents.
"
Class {
	#name : #ZLibWriteStream,
	#superclass : #ZipWriteStream,
	#category : 'Compression-Streams'
}

{ #category : #initialization }
ZLibWriteStream >> on: aCollectionOrStream [
	super on: aCollectionOrStream.
	crc := 1.
]

{ #category : #initialization }
ZLibWriteStream >> updateCrc: adler from: start to: stop in: aCollection [
	"Update crc using the Adler32 checksum technique from RFC1950"
	^ Adler32 update: adler from:  start to:  stop in: aCollection
]

{ #category : #initialization }
ZLibWriteStream >> writeFooter [
	"Store the Adler32 checksum as the last 4 bytes."
	3 to: 0 by: -1 do:[:i| encoder nextBytePut: (crc >> (i*8) bitAnd: 255)].
]

{ #category : #initialization }
ZLibWriteStream >> writeHeader [
	"Write header information"
	encoder nextBits: 8 put: 120. "deflate method with 15bit window size"
	encoder nextBits: 8 put: 94. "checksum; no preset; fast (flevel=1) compression"
]
