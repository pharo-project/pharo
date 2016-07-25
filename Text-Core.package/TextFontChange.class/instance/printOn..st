printing
printOn: strm
	super printOn: strm.
	strm nextPutAll: ' font: '; print: fontNumber