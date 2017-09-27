printing
imageVersionString
	"Print the version compatibile with naming the image."
	
	^String streamContents: [:str |
		str nextPutAll: self type;
			nextPut: $-;
			nextPutAll: self major printString;
			nextPut: $.;
			nextPutAll: self minor printString;
			nextPut: $-;
			nextPutAll: self suffix]