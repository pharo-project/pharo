FLBufferedWriteStream is a buffered write stream we use for Fuel serialization. Instead of directly using the stream provided to FLSerializer at creation time by the user, we create an instance of FLBufferedWriteStream for that stream.

MultiByteFileStream has no real buffer and goes to disk too frequently. With FLBufferedWriteStream we keep stuff in a cache and only go to disk when this is full.

The way of using it is jut FLBufferedWriteStream on: aWriteStream. For example:

FLBufferedWriteStream on: (FileDirectory default forceNewFileNamed:  'TestFile') binary

With the message #sizeBuffer: you can set the size of the buffer.

Make sure to always send #flush or #close when you're done, otherwise the last buffer might not yet have been written.
