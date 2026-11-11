"
I use the zlib implementation of the deflate algorithm to compress a ByteArray. According to Wikipedia, ""this algorithm provides good compression on a wide variety of data with minimal use of system resources."" See [http://en.wikipedia.org/wiki/DEFLATE](http://en.wikipedia.org/wiki/DEFLATE) for details on the deflate algorithm.

This is how you get a compressed ByteArray from your input:

```
zipStream := ZLibWriteStream on: (ByteArray new).
zipStream 
	nextPutAll: myByteArray;
	close.
compressed := zipStream encodedStream contents.
```
"
Class {
	#name : 'ZLibWriteStream',
	#superclass : 'ZipWriteStream',
	#instVars : [
		'dictionary'
	],
	#category : 'Compression-Streams',
	#package : 'Compression',
	#tag : 'Streams'
}

{ #category : 'instance creation' }
ZLibWriteStream class >> on: aCollectionOrStream dictionary: aByteArray [
	"Answer a stream compressing to ZLIB data with the preset dictionary aByteArray (RFC 1950, FDICT): data can refer to the dictionary, which is not written. It is needed to decompress (see ZLibReadStream class>>#on:dictionary:)."

	aByteArray size > 32768 ifTrue: [ ^ self error: 'Preset dictionary larger than the window' ].
	^ self basicNew
		  setDictionary: aByteArray;
		  on: aCollectionOrStream
]

{ #category : 'initialization' }
ZLibWriteStream >> on: aCollectionOrStream [

	super on: aCollectionOrStream.
	crc := 1.
	dictionary ifNotNil: [ self primeWithDictionary ]
]

{ #category : 'initialization' }
ZLibWriteStream >> primeWithDictionary [
	"Put the preset dictionary before the data, as if it had already been compressed, so that matches can refer to it"

	| size |
	size := dictionary size.
	collection
		replaceFrom: 1
		to: size
		with: (collection isString
				 ifTrue: [ dictionary asString ]
				 ifFalse: [ dictionary asByteArray ])
		startingAt: 1.
	1 to: MinMatch - 1 do: [ :index | self updateHashAt: index ].
	0 to: size - MinMatch do: [ :index | self insertStringAt: index ].
	position := blockPosition := blockStart := size.
	"The dictionary is neither part of the Adler-32 nor of the size of the data"
	crcPosition := size + 1
]

{ #category : 'initialization' }
ZLibWriteStream >> setDictionary: aByteArray [

	dictionary := aByteArray
]

{ #category : 'initialization' }
ZLibWriteStream >> updateCrc: adler from: start to: stop in: aCollection [
	"Update crc using the Adler32 checksum technique from RFC1950"
	^ Adler32 update: adler from:  start to:  stop in: aCollection
]

{ #category : 'initialization' }
ZLibWriteStream >> writeFooter [
	"Store the Adler32 checksum as the last 4 bytes."
	3 to: 0 by: -1 do:[:i| encoder nextBytePut: (crc >> (i*8) bitAnd: 255)]
]

{ #category : 'initialization' }
ZLibWriteStream >> writeHeader [
	"Write header information: deflate method with 15bit window size, fast (flevel=1) compression, the preset dictionary flag and the check bits (RFC 1950, section 2.2)"

	| flags |
	flags := 1 bitShift: 6.
	dictionary ifNotNil: [ flags := flags bitOr: 32 ].
	flags := flags + (31 - (120 * 256 + flags \\ 31) \\ 31).
	encoder nextBits: 8 put: 120.
	encoder nextBits: 8 put: flags.
	dictionary ifNotNil: [
			| dictionaryId |
			dictionaryId := Adler32
				                update: 1
				                from: 1
				                to: dictionary size
				                in: dictionary asByteArray.
			3 to: 0 by: -1 do: [ :index | encoder nextBits: 8 put: ((dictionaryId bitShift: index * -8) bitAnd: 255) ] ]
]
