testing
atEndOfFile
	"Answer whether the receiver is at its end based on the result of
	the last read operation. This uses feof() to test the underlying file
	stream status, and can be used as an alternative to #atEnd, which
	does not properly report end of file status for an OSSPipe."

	^ reader atEndOfFile
