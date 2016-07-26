stream-protocol
writeStreamDo: aBlock
	| stream |
	stream := WriteStream on: ByteArray new.
	[ aBlock value: stream ] ensure: [ stream close ].
	bytes := stream contents.
	self updateSize: bytes size