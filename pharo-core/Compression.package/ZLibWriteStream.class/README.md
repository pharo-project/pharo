I use the zlib implementation of the deflate algorithm to compress a ByteArray. According to Wikipedia, "this algorithm provides good compression on a wide variety of data with minimal use of system resources." See http://en.wikipedia.org/wiki/DEFLATE for details on the deflate algorithm.

This is how you get a compressed ByteArray from your input:
zipStream := ZLibWriteStream on: (ByteArray new).
zipStream 
	nextPutAll: myByteArray;
	close.
compressed := zipStream encodedStream contents.