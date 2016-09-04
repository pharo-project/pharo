positioning
skip: n
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file.
	By default we read n characters and we avoid reading the output"
	self next: n