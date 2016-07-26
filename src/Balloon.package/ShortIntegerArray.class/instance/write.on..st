objects from disk
write: shortInteger on: aStream
	^ EndianDetector isLittleEndian
		ifFalse: [ self write: shortInteger bigEndianOn: aStream ]
		ifTrue: [ self write: shortInteger littleEndianOn: aStream ]