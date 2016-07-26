Provides a simple, platform-independent, interface to a file system. The instance variable rwmode, inherited from class PositionableStream, here is used to hold a Boolean -- true means opened for read-write, false means opened for read-only.  2/12/96 sw

I implement a simple read buffering scheme with the variables defined in PositionableStream (which are unused in me otherwise) in the following way:
	collection	<ByteString> or <ByteArray>	This is the buffer.
	position	<Integer>	The relative position in the buffer. Greater or equal to zero.
	readLimit	<Integer>	The number of bytes buffered. Greater or equal to zero.
Read buffering is enabled with #enableReadBuffering, disabled with #disableReadBuffering and it is enabled by default. The buffer is filled when a read attempt of an unbuffered absolute position is requested, or when a negative repositioning is made (with #position: with an argument < than the current absolute position) to an absolute position which is not buffered. In the first case, the buffer is positioned to the given absolute position. In the latter case the repositioning is made to the requested absolute position minus fourth of the buffer size. This means that further small negative repositionings won't result in buffer flushing. This is really useful when filing in code.
The read buffer is flushed (#flushReadBuffer) whenever a write attempt is made.
The buffer state is valid if and only if collection is not nil and position < readLimit.