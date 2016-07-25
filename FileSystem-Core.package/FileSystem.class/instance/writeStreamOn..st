public
writeStreamOn: aResolvable
	"Open a write stream on the file referred by the argument. It can be a string or a path"

	^ (self open: aResolvable writable: true) writeStream.