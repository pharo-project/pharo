initialize-release
initializeBuffers
	"The maximum payload message length of a TLS record is 16Kb,
	add a margin for the header and trailer."
	
	readBuffer := ZdcIOBuffer onByteArrayOfSize: 16 * 1024.
	writeBuffer := ZdcIOBuffer onByteArrayOfSize: 4 * 1024.
	in := ZdcByteArrayManager current byteArrayOfSize: 4096 zero: false.
	out := ZdcByteArrayManager current byteArrayOfSize: (16 + 1) * 1024 zero: false
