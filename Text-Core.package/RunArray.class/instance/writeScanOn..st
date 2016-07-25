printing
writeScanOn: strm
	"Write out the format used for text runs in source files. (14 50 312)f1,f1b,f1LInteger +;i"

	strm nextPut: $(.
	runs do: [:rr | rr printOn: strm. strm space].
	strm skip: -1; nextPut: $).
	values do: [:vv |
		vv do: [:att | att writeScanOn: strm].
		strm nextPut: $,].
	strm skip: -1. "trailing comma"