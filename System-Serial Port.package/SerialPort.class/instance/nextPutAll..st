input/output
nextPutAll: aStringOrByteArray 
	"Send the given bytes out this serial port. The port must be
	open. "
	^ port isString
		ifTrue: [self
					primWritePortByName: port
					from: aStringOrByteArray
					startingAt: 1
					count: aStringOrByteArray size]
		ifFalse: [self
					primWritePort: port
					from: aStringOrByteArray
					startingAt: 1
					count: aStringOrByteArray size]
