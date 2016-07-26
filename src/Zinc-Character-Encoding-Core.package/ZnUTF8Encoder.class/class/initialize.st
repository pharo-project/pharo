class initialization
initialize
	| encoder stream |
	ByteASCIISet := ByteArray new: 256.
	ByteUTF8Encoding := Array new: 256.
	encoder := self new.
	stream := ByteArray new writeStream.
	0 to: 255 do: [ :each |
		| bytes |
		stream reset.
		encoder nextPut: (Character value: each) toStream: stream.
		bytes := stream contents.
		(bytes size = 1 and: [ bytes first = each ])
			ifTrue: [
				ByteASCIISet at: each + 1 put: 0 ]
			ifFalse: [  
				ByteASCIISet at: each + 1 put: 1.
				ByteUTF8Encoding at: each + 1 put: bytes ] ] 