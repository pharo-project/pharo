as yet unclassified
writeScanOn: strm

	strm nextPut: $F.
	strm nextPutAll: font familyName; nextPut: $#.
	font height printOn: strm.