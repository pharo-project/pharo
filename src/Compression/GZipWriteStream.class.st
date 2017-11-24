"
gzip is an integral part of the VM. 
"
Class {
	#name : #GZipWriteStream,
	#superclass : #ZipWriteStream,
	#pools : [
		'GZipConstants'
	],
	#category : #'Compression-Streams'
}

{ #category : #initialization }
GZipWriteStream >> writeFooter [
	"Write some footer information for the crc"
	super writeFooter.
	0 to: 3 do:[:i| encoder nextBytePut: (crc >> (i*8) bitAnd: 255)].
	0 to: 3 do:[:i| encoder nextBytePut: (bytesWritten >> (i*8) bitAnd: 255)].
]

{ #category : #initialization }
GZipWriteStream >> writeHeader [
	"Write the GZip header"
	encoder nextBits: 16 put: GZipMagic.
	encoder nextBits: 8 put: GZipDeflated.
	encoder nextBits: 8 put: 0. "No flags"
	encoder nextBits: 32 put: 0. "no time stamp"
	encoder nextBits: 8 put: 0. "No extra flags"
	encoder nextBits: 8 put: 0. "No OS type"

]
