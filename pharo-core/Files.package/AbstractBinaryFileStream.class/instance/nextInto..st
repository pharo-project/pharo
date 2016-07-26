reading
nextInto: aBuffer
	"Return a string with the next n characters of the filestream in it."

	^ self next: aBuffer size into: aBuffer