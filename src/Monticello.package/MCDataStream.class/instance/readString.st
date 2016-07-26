write and read
readString

	| str |
	byteStream ascii.
	str := byteStream nextString.
	byteStream binary.
	^ str
