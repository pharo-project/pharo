*Graphics-Files
writeOnMovie: file
	"Write just my bits on the file."
	self unhibernate.
	bits writeUncompressedOn: file