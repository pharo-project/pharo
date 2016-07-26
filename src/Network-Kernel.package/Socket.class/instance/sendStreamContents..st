sending
sendStreamContents: stream
	"Send the data in the stream. Close the stream.
	Usefull for directly sending contents of a file without reading into memory first."

	self sendStreamContents: stream checkBlock: [true]