reading
next: n
	"Return a string with the next n characters of the filestream in it."

	^ self next: n into: (ByteArray new: n)