*Monticello-OldDataStreamCompatibility
readFloat
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	 We support 8-byte Floats here.  Non-IEEE"

	| new |
	new := Float new: 2.		"To get an instance"
	new at: 1 put: (byteStream nextNumber: 4).
	new at: 2 put: (byteStream nextNumber: 4).
	^ new