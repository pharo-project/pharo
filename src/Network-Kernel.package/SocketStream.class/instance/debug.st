printing
debug
	"Display debug info."

	| data |
	data := self inBufferSize.
	^String streamContents: [:s |
		s
			nextPutAll: 'Buffer size: ', inBuffer size asString;cr;
			nextPutAll: 'InBuffer data size: ', data asString; cr;
			nextPutAll: 'In data (20):', (inBuffer copyFrom: lastRead + 1 to: lastRead + (data min: 20)); cr;
			nextPutAll: 'OutBuffer data size: ', (outNextToWrite - 1) asString; cr;
			nextPutAll: 'Out data (20):', (outBuffer copyFrom: 1 to: ((outNextToWrite - 1) min: 20)); cr]