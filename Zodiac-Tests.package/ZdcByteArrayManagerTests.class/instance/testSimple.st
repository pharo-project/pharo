testing
testSimple
	| byteArrayManager byteArray |
	byteArrayManager := ZdcByteArrayManager new.
	byteArrayManager limit: 1.
	self assert: byteArrayManager totalSize equals: 0.
	byteArray := byteArrayManager byteArrayOfSize: 4096 zero: false.
	byteArrayManager recycle: byteArray.
	self assert: byteArrayManager totalSize equals: 4096.
	self assert: (byteArrayManager byteArrayOfSize: 4096 zero: false) == byteArray