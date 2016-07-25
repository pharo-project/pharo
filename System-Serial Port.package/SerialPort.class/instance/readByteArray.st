input/output
readByteArray
	"Answer a ByteArray read from this serial port. Answer an empty ByteArray if no data is available. The port must be open. "
	
	| buf count |
	buf := ByteArray new: 1000.
	count := port isString
				ifTrue: [self
						primReadPortByName: port
						into: buf
						startingAt: 1
						count: buf size]
				ifFalse: [self
						primReadPort: port
						into: buf
						startingAt: 1
						count: buf size].
	^ buf copyFrom: 1 to: count