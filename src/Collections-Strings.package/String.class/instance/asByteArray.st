converting
asByteArray
	"Convert to a ByteArray with the ascii values of the string."
	| b |
	b := ByteArray new: self byteSize.
	1 to: self size * 4 do: [:i |
		b at: i put: (self byteAt: i)].
	^ b.
