instance creation
onByteArrayOfSize: size
	^ self on: (ZdcByteArrayManager current byteArrayOfSize: size zero: true)