testing
testClearing
	| byteArrayManager one two |
	byteArrayManager := ZdcByteArrayManager new.
	byteArrayManager limit: 1.
	self assert: byteArrayManager totalSize equals: 0.
	one := (1 to: 10) asByteArray.
	byteArrayManager recycle: one.
	two := byteArrayManager byteArrayOfSize: 10 zero: true.
	self assert: one == two.
	self assert: two equals: (ByteArray new: 10)