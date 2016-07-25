initialization
contents: aString
	contents := aString.
	compressedSize := uncompressedSize := aString size.
	"set the file date to now"
	self modifiedAt: DateAndTime now.