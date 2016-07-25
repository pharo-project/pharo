utilities
pixelsPerInch
	"Answer the nominal resolution of the screen."

	^TextSharedInformation at: #pixelsPerInch ifAbsentPut: [ 96.0 ].