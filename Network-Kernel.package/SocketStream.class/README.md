SocketStream is a wrapper for class Socket making it easy to write networking code by giving the programmer a stream-like protocol. A Socket is a two way communication link with two logically separate channels - input and output. The Socket class is the lowest level in Pharo for network communication and using it directly can be difficult and bug prone.

A SocketStream can be in binary or ascii mode, ascii is the default which means you are transmitting and receiving Strings. Most Internet protocols are in clear text ascii, like for example HTTP. Another setting is what timeout you want to use - default is the standardTimeout from Socket. More settings can be found in the method category 'configuration'.

Simplest example of connecting, sending/receiving and closing:

| stream result |
stream := SocketStream openConnectionToHostNamed: 'www.pharo-project.org' port: 80.
[[stream nextPutAll: 'GET / HTTP/1.0'; crlf; crlf; flush.
result := stream upToEnd. "Give us all data until the socket is closed."
Transcript show: result; cr.]
	ensure: [stream close]]
		on: ConnectionTimedOut
		do: [:ex | Transcript show: ex asString;cr. ex resume]

There are two important things to note above:
	- The methods in category "stream in" can signal two exceptions (unless turned off with #shouldSignal:):
		ConnectionClosed and ConnectionTimedOut
	- We close the stream using #ensure:, that is to make sure it isn't left opened.
	- We use #on:do: to catch any signal. In this case we do not need to catch ConnectionClosed since #upToEnd does that for us intrinsically.

----------------
SocketStream (below called SS) is a reimplementation of 'Old'-SocketStream (below called OSS) - the class that originates from the original Comanche implementation but now is included in standard Squeak. SS has the same protocol as OSS and is meant to replace it. SS is faster, more flexible, is better documented and adds a few features:

1. #shouldSignal:, which decides if SS should signal low level Socket exceptions (true) or if it should swallow them like original OSS did. Default is true. The only reason I added this is for backwards compatibility - not signalling causes problems - see bug 4 below.

2. #nextAllInBuffer, #nextInBuffer:, #skip:, #receiveData:, #nextPutAllFlush: and #recentlyRead are new additions to the public protocol.


It also fixes various bugs:

1. #isDataAvailable could theoretically answer false, when there actually is some in the buffer in OSS. If #receiveDataIfAvailable reads the last byte then the following "socket dataAvailable" would answer false. So the last byte would be sitting in the inStream missed.

2. #upToAll: in OSS has several problems, for example - #positionOfSubCollection:ifAbsent: which was introduced answers one position too low. This was compensated in upToAll:, but only in the pushBack: call, not the actual result being returned which was cut short 1 byte. Amusingly this makes KomHttpServer not use "Keep-Alive" since the last $e in 'Alive' was cut short. :)

3. SS doesn't inherit from PositionableStream since that just breaks various inherited messages, like for example #skip:. OSS should IMHO be changed to inherit from Object - or of course, replaced in full with SS. :)

4. Since SocketStream by default signals closes and timeouts the SocketStreamTest now passes. The reason for SocketStream to fail is that while it does timeout on a low level (#SocketStream>>receiveData doesn't hang forever) - the callers of #receiveData sometimes loop - like in #next:, and thus eliminates the timeout. SS warns about some methods (in their method comments) not honouring timeouts if shouldSignal is false, I really don't know what they should do in that case:
	#next:, #upTo:, #upToAll: and #upToEnd (and #receiveData:)


The primary reason for the SS implementation is optimal performance. The main differences in implementation with the old OSS are:

1. SS uses two buffers directly (inBuffer and outBuffer) with pointers marking start and stop within the buffer. OSS instead uses two regular streams, a ReadStream and a WriteStream. Using internal buffers makes it possible to avoid copying and reallocation in various ways, it also makes SS be able to have specialized growing/buffer moving behaviour.

2. #upTo:, #upToAll: and #peekForAll: uses selectged String messages that in turn uses fast primitives for searching. OSS used other messages that fell back on byte per byte reading.

3. #receiveData in OSS creates a temporary buffer stream for each call! During a long read operation, like say #upToAll: (which for example is used when uploading files using HTTP POST forms), this is devastating - especially since the default size is only 2000 bytes - and leads to a very high number of low level read operations on the Socket, typically 100 times more calls than with OSS. The buffer in OSS is held in an instvar (not recreated for each call), is larger from the start and above all - grows dynamically by doubling. OSS can also avoid a grow/reallocation by doing a "move down" if data has been read from the SS as it comes in and through that making room in the lower part of the inBuffer. The net result is that upToAll: for large files is about 10 times faster.

4. The implementation of upTo: and upToAll: tries to avoid doing unnecessary find operations in the buffer and is greedy by default, which means it favors reading more data - if available - before searching for the stop sequence. If we had #findString:startingAt:stoppingAt: this wouldn't have to be greedy and we wouldn't be needlessly scanning dead buffer area. VM hackers? Also, while you are at it - make it work for ByteArrays too. :)


SS can not be run unbuffered, since that seems unneeded. The option to autoFlush is still available, with it set to true SocketStream (just like OSS) will flush on its own on each nextPut:/nextPutAll:, otherwise flushing it will have to be done manually but is done on close.

The first performance tests shows that, as noted above, receiving large amounts of data using #upToAll: is greatly improved - factor of 10. Serving HTTP with small payloads seemed at first not be faster at all - but this is due to the high overhead of Socket connect/close and other things. Increasing payloads show a difference and especially with keep alive on - where the new SS roughly doubles the throughput!