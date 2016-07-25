decoding
decodeSourcePointer

	"Trailer is a source pointer"
	| msz |
	
	size := 4.
	msz := method size.
	data := (method at: msz) - 251 << 8 + (method at: msz-1) << 8
	 + (method at: msz-2) << 8 + (method at: msz-3).
	
