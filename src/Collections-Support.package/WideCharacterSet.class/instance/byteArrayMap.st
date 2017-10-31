private
byteArrayMap
	"returns a ByteArray mapping n < 256 to 1 if (Character asciValue: (n-1)) is in the set, 
	and  0 if it isn't.
	Intended for internal use, and for comparison."
	
	^ byteArrayMap