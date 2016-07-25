private
convertCursor: aCursor
	| result bits|
	aCursor unhibernate.
	bits := aCursor bits.
	result := ByteArray new: 32.
	0 to: 15 do: [ :i |
		result at: i*2 + 1 put: (bits at: i + 1) >> 24 & 255.
		result at: i*2 + 2 put: (bits at: i + 1) >> 16 & 255.
	].
	^ result