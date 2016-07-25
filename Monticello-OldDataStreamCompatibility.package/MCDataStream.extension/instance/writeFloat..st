*Monticello-OldDataStreamCompatibility
writeFloat: aFloat
	"PRIVATE -- Write the contents of a Float.
	  We support 8-byte Floats here."

	byteStream nextNumber: 4 put: (aFloat at: 1).
	byteStream nextNumber: 4 put: (aFloat at: 2).
