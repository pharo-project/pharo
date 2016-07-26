resources
storeResourceOn: aStream
	"Store a resource representation of the receiver on aStream.
	Must be specific to the receiver so that no code is filed out."
	self hibernate.
	aStream nextPutAll: self resourceTag asByteArray. "tag"
	aStream nextNumber: 4 put: width.
	aStream nextNumber: 4 put: height.
	aStream nextNumber: 4 put: depth.
	(bits isMemberOf: ByteArray) ifFalse:
		"must store bitmap"
		[aStream nextNumber: 4 put: 0. "tag"
		aStream nextNumber: 4 put: (EndianDetector isBigEndian ifTrue:[1] ifFalse: [0])].
	aStream nextNumber: 4 put: bits size.
	aStream nextPutAll: bits.
