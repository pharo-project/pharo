*Monticello-OldDataStreamCompatibility
readShortRef
	"Read an object reference from two bytes only.  Original object must be in first 65536 bytes of the file.  Relative to start of data.  vacantRef not a possibility."

	^ self objectAt: (byteStream nextNumber: 2)