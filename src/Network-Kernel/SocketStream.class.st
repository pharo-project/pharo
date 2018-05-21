"
SocketStream is a wrapper for class Socket making it easy to write networking code by giving the programmer a stream-like protocol. A Socket is a two way communication link with two logically separate channels - input and output. The Socket class is the lowest level in Pharo for network communication and using it directly can be difficult and bug prone.

A SocketStream can be in binary or ascii mode, ascii is the default which means you are transmitting and receiving Strings. Most Internet protocols are in clear text ascii, like for example HTTP. Another setting is what timeout you want to use - default is the standardTimeout from Socket. More settings can be found in the method category 'configuration'.

Simplest example of connecting, sending/receiving and closing:

| stream result |
stream := SocketStream openConnectionToHostNamed: 'www.pharo-project.org' port: 80.
[[stream nextPutAll: 'GET / HTTP/1.0'; crlf; crlf; flush.
result := stream upToEnd. ""Give us all data until the socket is closed.""
Transcript show: result; cr.]
	ensure: [stream close]]
		on: ConnectionTimedOut
		do: [:ex | Transcript show: ex asString;cr. ex resume]

There are two important things to note above:
	- The methods in category ""stream in"" can signal two exceptions (unless turned off with #shouldSignal:):
		ConnectionClosed and ConnectionTimedOut
	- We close the stream using #ensure:, that is to make sure it isn't left opened.
	- We use #on:do: to catch any signal. In this case we do not need to catch ConnectionClosed since #upToEnd does that for us intrinsically.

----------------
SocketStream (below called SS) is a reimplementation of 'Old'-SocketStream (below called OSS) - the class that originates from the original Comanche implementation but now is included in standard Squeak. SS has the same protocol as OSS and is meant to replace it. SS is faster, more flexible, is better documented and adds a few features:

1. #shouldSignal:, which decides if SS should signal low level Socket exceptions (true) or if it should swallow them like original OSS did. Default is true. The only reason I added this is for backwards compatibility - not signalling causes problems - see bug 4 below.

2. #nextAllInBuffer, #nextInBuffer:, #skip:, #receiveData:, #nextPutAllFlush: and #recentlyRead are new additions to the public protocol.


It also fixes various bugs:

1. #isDataAvailable could theoretically answer false, when there actually is some in the buffer in OSS. If #receiveDataIfAvailable reads the last byte then the following ""socket dataAvailable"" would answer false. So the last byte would be sitting in the inStream missed.

2. #upToAll: in OSS has several problems, for example - #positionOfSubCollection:ifAbsent: which was introduced answers one position too low. This was compensated in upToAll:, but only in the pushBack: call, not the actual result being returned which was cut short 1 byte. Amusingly this makes KomHttpServer not use ""Keep-Alive"" since the last $e in 'Alive' was cut short. :)

3. SS doesn't inherit from PositionableStream since that just breaks various inherited messages, like for example #skip:. OSS should IMHO be changed to inherit from Object - or of course, replaced in full with SS. :)

4. Since SocketStream by default signals closes and timeouts the SocketStreamTest now passes. The reason for SocketStream to fail is that while it does timeout on a low level (#SocketStream>>receiveData doesn't hang forever) - the callers of #receiveData sometimes loop - like in #next:, and thus eliminates the timeout. SS warns about some methods (in their method comments) not honouring timeouts if shouldSignal is false, I really don't know what they should do in that case:
	#next:, #upTo:, #upToAll: and #upToEnd (and #receiveData:)


The primary reason for the SS implementation is optimal performance. The main differences in implementation with the old OSS are:

1. SS uses two buffers directly (inBuffer and outBuffer) with pointers marking start and stop within the buffer. OSS instead uses two regular streams, a ReadStream and a WriteStream. Using internal buffers makes it possible to avoid copying and reallocation in various ways, it also makes SS be able to have specialized growing/buffer moving behaviour.

2. #upTo:, #upToAll: and #peekForAll: uses selectged String messages that in turn uses fast primitives for searching. OSS used other messages that fell back on byte per byte reading.

3. #receiveData in OSS creates a temporary buffer stream for each call! During a long read operation, like say #upToAll: (which for example is used when uploading files using HTTP POST forms), this is devastating - especially since the default size is only 2000 bytes - and leads to a very high number of low level read operations on the Socket, typically 100 times more calls than with OSS. The buffer in OSS is held in an instvar (not recreated for each call), is larger from the start and above all - grows dynamically by doubling. OSS can also avoid a grow/reallocation by doing a ""move down"" if data has been read from the SS as it comes in and through that making room in the lower part of the inBuffer. The net result is that upToAll: for large files is about 10 times faster.

4. The implementation of upTo: and upToAll: tries to avoid doing unnecessary find operations in the buffer and is greedy by default, which means it favors reading more data - if available - before searching for the stop sequence. If we had #findString:startingAt:stoppingAt: this wouldn't have to be greedy and we wouldn't be needlessly scanning dead buffer area. VM hackers? Also, while you are at it - make it work for ByteArrays too. :)


SS can not be run unbuffered, since that seems unneeded. The option to autoFlush is still available, with it set to true SocketStream (just like OSS) will flush on its own on each nextPut:/nextPutAll:, otherwise flushing it will have to be done manually but is done on close.

The first performance tests shows that, as noted above, receiving large amounts of data using #upToAll: is greatly improved - factor of 10. Serving HTTP with small payloads seemed at first not be faster at all - but this is due to the high overhead of Socket connect/close and other things. Increasing payloads show a difference and especially with keep alive on - where the new SS roughly doubles the throughput!
"
Class {
	#name : #SocketStream,
	#superclass : #Object,
	#instVars : [
		'recentlyRead',
		'socket',
		'inBuffer',
		'outBuffer',
		'inNextToWrite',
		'outNextToWrite',
		'lastRead',
		'timeout',
		'autoFlush',
		'bufferSize',
		'binary',
		'shouldSignal'
	],
	#category : #'Network-Kernel-Base'
}

{ #category : #example }
SocketStream class >> finger: userName [
	"SocketStream finger: 'stp'"

	| addr s |
	addr := NetNameResolver promptUserForHostAddress.
	s := SocketStream openConnectionToHost: addr port: 79.  "finger port number"
	Transcript show: '---------- Connecting ----------'; cr.
	s sendCommand: userName.
	Transcript show: s nextLineCrLf.
	s close.
	Transcript show: '---------- Connection Closed ----------'; cr; endEntry.

]

{ #category : #'instance creation' }
SocketStream class >> on: socket [
	"Create a socket stream on a connected server socket."

	^self basicNew initialize socket: socket
]

{ #category : #'instance creation' }
SocketStream class >> openConnectionToHost: hostIP port: portNumber [
	^ self openConnectionToHost: hostIP port: portNumber timeout: Socket standardTimeout

]

{ #category : #'instance creation' }
SocketStream class >> openConnectionToHost: hostIP port: portNumber timeout: timeout [
	| socket |
	socket := Socket new.
	socket connectTo: hostIP port: portNumber waitForConnectionFor: timeout.
	^self on: socket
]

{ #category : #'instance creation' }
SocketStream class >> openConnectionToHostNamed: hostName port: portNumber [
	| hostIP |
	hostIP := NetNameResolver addressForName: hostName timeout: 20.
	^self openConnectionToHost: hostIP port: portNumber
]

{ #category : #private }
SocketStream >> << items [

 	items putOn: self.
	
	^ self
]

{ #category : #private }
SocketStream >> adjustInBuffer: bytesRead [
	"Adjust markers and possibly grow inBuffer or move data down.
	Currently grows through doubling when less than 1024 bytes are left.
	Never shrinks. Returns the position in the buffer where any new
	data can be found."

	| old |
	bytesRead = 0 ifTrue: [^inNextToWrite].
	old := inNextToWrite.
	inNextToWrite := inNextToWrite + bytesRead.
	(inBuffer size - inNextToWrite) < 1024
		ifTrue: [
			"Hit the roof, move data down (if enough has been read) or do we grow?"
			(lastRead > 512)
				ifTrue: [^old - self moveInBufferDown]
				ifFalse: [self growInBuffer]].
	^old
]

{ #category : #private }
SocketStream >> adjustOutBuffer: bytesToWrite [
	"Possibly grow outBuffer to accommodate the new data.
	Currently grows through doubling when less
	than 1024 bytes are left. If bytesToWrite is even
	larger we double that instead. Never shrinks."

	(outBuffer size - outNextToWrite - bytesToWrite) < 1024 ifTrue: [
		outBuffer := (self streamBuffer: ((outBuffer size max: bytesToWrite) * 2))
						replaceFrom: 1 to: outBuffer size with: outBuffer startingAt: 1]
]

{ #category : #configuration }
SocketStream >> ascii [
	"Tell the SocketStream to send data
	as Strings instead of ByteArrays.
	This is default."

	binary := false.
	inBuffer
		ifNil: [self resetBuffers]
		ifNotNil:
			[inBuffer := inBuffer asString.
			outBuffer := outBuffer asString]
]

{ #category : #testing }
SocketStream >> atEnd [
	"There is nothing more to read when
	there is no more data in our inBuffer, the socket
	is disconnected and there is none available on the socket.
	Note that we need to check isConnected before isDataAvailable,
	otherwise data may sneak in in the meantime. But we check the
	buffer first, because it is faster."

	self isInBufferEmpty ifFalse: [^false].
	^self isConnected not
		and: [self isDataAvailable not]
]

{ #category : #configuration }
SocketStream >> autoFlush [
	"If autoFlush is enabled data will be sent through
	the socket (flushed) when the bufferSize is reached
	or the SocketStream is closed. Otherwise the user
	will have to send #flush manually.
	Close will always flush. Default is false."

	^autoFlush
]

{ #category : #configuration }
SocketStream >> autoFlush: aBoolean [
	"If autoFlush is enabled data will be sent through
	the socket (flushed) when the bufferSize is reached
	or the SocketStream is closed. Otherwise the user
	will have to send #flush manually.
	Close will always flush. Default is false."

	autoFlush := aBoolean
]

{ #category : #configuration }
SocketStream >> binary [
	"Tell the SocketStream to send data
	as ByteArrays instead of Strings.
	Default is ascii."

	binary := true.
	inBuffer
		ifNil: [self resetBuffers]
		ifNotNil:
			[inBuffer := inBuffer asByteArray.
			outBuffer := outBuffer asByteArray]
]

{ #category : #configuration }
SocketStream >> bufferSize [
	"Default buffer size is 4kb.
	increased from earlier 2000 bytes."
	
	^bufferSize
]

{ #category : #configuration }
SocketStream >> bufferSize: anInt [
	"Default buffer size is 4kb.
	increased from earlier 2000 bytes."

	bufferSize := anInt
]

{ #category : #private }
SocketStream >> checkFlush [
	"If autoFlush is true we flush if
	we have reached the bufferSize
	of data in the outBuffer."

	(autoFlush and: [outNextToWrite > bufferSize])
		ifTrue: [self flush]
]

{ #category : #control }
SocketStream >> close [
	"Flush any data still not sent
	and take care of the socket."

	self flush.
	socket closeAndDestroy: 30
]

{ #category : #'stream out' }
SocketStream >> cr [
	self nextPutAll: String cr
]

{ #category : #'stream out' }
SocketStream >> crlf [
	self nextPutAll: String crlf
]

{ #category : #printing }
SocketStream >> debug [
	"Display debug info."

	| data |
	data := self inBufferSize.
	^String streamContents: [:s |
		s
			nextPutAll: 'Buffer size: ', inBuffer size asString;cr;
			nextPutAll: 'InBuffer data size: ', data asString; cr;
			nextPutAll: 'In data (20):', (inBuffer copyFrom: lastRead + 1 to: lastRead + (data min: 20)); cr;
			nextPutAll: 'OutBuffer data size: ', (outNextToWrite - 1) asString; cr;
			nextPutAll: 'Out data (20):', (outBuffer copyFrom: 1 to: ((outNextToWrite - 1) min: 20)); cr]
]

{ #category : #control }
SocketStream >> destroy [
	"Destroy the receiver and its underlying socket. Does not attempt to flush the output buffers. For a graceful close use SocketStream>>close instead."
	socket ifNotNil: [socket destroy]
]

{ #category : #control }
SocketStream >> flush [
	"If the other end is connected and we have something
	to send, then we send it and reset the outBuffer."

	((outNextToWrite > 1) and: [socket isOtherEndClosed not])
		ifTrue: [
			[socket sendData: outBuffer count: outNextToWrite - 1]
				on: ConnectionTimedOut
				do: [:ex | shouldSignal ifFalse: ["swallow"]].
			outNextToWrite := 1]
]

{ #category : #private }
SocketStream >> growInBuffer [
	"Grows through doubling."

	self resizeInBuffer: inBuffer size * 2
]

{ #category : #testing }
SocketStream >> ifStale: aBlock [
	self isConnected
		ifFalse: aBlock
]

{ #category : #configuration }
SocketStream >> inBufferSize [
	"Answers the current size of data in the inBuffer."

	^inNextToWrite - lastRead - 1
]

{ #category : #initialization }
SocketStream >> initialize [
	super initialize.
	autoFlush := true.
	shouldSignal := true.
	recentlyRead := 0.
	bufferSize := 4096.
	self ascii
]

{ #category : #testing }
SocketStream >> isBinary [
	^binary
]

{ #category : #testing }
SocketStream >> isConnected [
	"The stream is connected if the socket is."

	^socket isConnected
]

{ #category : #testing }
SocketStream >> isDataAvailable [
	"It the inbuffer is empty, we check the socket for data.
	If it claims to have data available to read, we try to read
	some once and recursively call this method again.
	If something really was available it is now in the inBuffer.
	This is because there has been spurious
	dataAvailable when there really is no data to get."
 
	self isInBufferEmpty ifFalse: [^true].
	^socket dataAvailable
		ifFalse: [false]
		ifTrue: [self receiveDataIfAvailable; isDataAvailable]
]

{ #category : #testing }
SocketStream >> isEmpty [
	"Test if there are more data to read."

	^self isInBufferEmpty and: [self isDataAvailable not]
]

{ #category : #testing }
SocketStream >> isInBufferEmpty [
	"Any data in the buffer?"
 
	^lastRead + 1 = inNextToWrite
]

{ #category : #testing }
SocketStream >> isOtherEndConnected [
	^socket isOtherEndClosed not
]

{ #category : #testing }
SocketStream >> isStream [
	^true
]

{ #category : #private }
SocketStream >> moveInBufferDown [
	"Move down contents of inBuffer to the start.
	Return distance moved."

	| sz distanceMoved |
	sz := inNextToWrite - lastRead - 1.
	inBuffer replaceFrom: 1 to: sz with: inBuffer startingAt: lastRead + 1.
	distanceMoved := lastRead.
	lastRead := 0.
	inNextToWrite := sz + 1.
	^distanceMoved

]

{ #category : #'stream in' }
SocketStream >> next [
	"Return next byte, if inBuffer is empty
	we recieve some more data and try again."

	self atEnd ifTrue: [^nil].
	self isInBufferEmpty ifTrue:
		[self receiveData.
		self atEnd ifTrue: [^nil]].
	lastRead := lastRead + 1.
	^inBuffer at: lastRead
]

{ #category : #'stream in' }
SocketStream >> next: anInteger [
	"Read count elements and return them in a collection.
	If the receiver is #atEnd before count elements were read, 
	return a smaller collection and don't signal ConnectionClosed."

	^ self nextInto: (self streamBuffer: anInteger)
]

{ #category : #'stream in' }
SocketStream >> next: n into: aCollection [
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."
	^self next: n into: aCollection startingAt: 1
]

{ #category : #'stream in' }
SocketStream >> next: requestedCount into: collection startingAt: startIndex [
	"Read requestedCount objects into the given collection. 
	Return collection or a partial copy if less elements have been read."

	| readCount |
	readCount := self readInto: collection startingAt: startIndex count: requestedCount.
	^ readCount = requestedCount
		ifTrue: [ ^ collection ]
		ifFalse: [ ^ collection copyFrom: 1 to: startIndex + readCount - 1 ]
]

{ #category : #'stream out' }
SocketStream >> next: n putAll: aCollection startingAt: startIndex [
	"Put a String or a ByteArray onto the stream.
	Currently a large collection will allocate a large buffer.
	Warning: this does not work with WideString: they have to be converted first."

	self adjustOutBuffer: n.
	outBuffer replaceFrom: outNextToWrite to: outNextToWrite + n - 1 with: aCollection startingAt: startIndex.
	outNextToWrite := outNextToWrite + n.
	self checkFlush.
	^aCollection
]

{ #category : #'stream in' }
SocketStream >> nextAllInBuffer [
	"Return all data currently in the inBuffer,"

	^self nextInBuffer: inNextToWrite - lastRead - 1
]

{ #category : #'stream in' }
SocketStream >> nextAvailable [
	"Answer all the data currently available,
	in buffer or in socket."

	self isInBufferEmpty ifFalse: [^self nextAllInBuffer].
	self isDataAvailable ifTrue: [self receiveData].
	^self nextAllInBuffer
]

{ #category : #'stream in' }
SocketStream >> nextAvailable: howMany [
	"Answer all the data currently available,
	in buffer or in socket - but limited to <howMany>."

	self isInBufferEmpty ifFalse: [^self nextInBuffer: howMany].
	self isDataAvailable ifTrue: [self receiveData].
	^self nextInBuffer: howMany
]

{ #category : #'stream in' }
SocketStream >> nextInBuffer: anInteger [
	"Answer anInteger bytes of data at most,
	but only from the inBuffer."

	| start amount |
	amount := anInteger min: (inNextToWrite - lastRead - 1).
	start := lastRead + 1.
	lastRead := lastRead + amount.
	^inBuffer copyFrom: start to: lastRead
]

{ #category : #'stream in' }
SocketStream >> nextInto: aCollection [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.
]

{ #category : #'stream in' }
SocketStream >> nextInto: aCollection startingAt: startIndex [
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: (aCollection size - startIndex+1) into: aCollection startingAt: startIndex.
]

{ #category : #'stream in' }
SocketStream >> nextLine [
	^self nextLineCrLf
]

{ #category : #'stream in' }
SocketStream >> nextLineCrLf [
	^self upToAll: String crlf
]

{ #category : #'stream in' }
SocketStream >> nextLineLf [
	| nextLine |
	nextLine := self upToAll: String lf.
	^nextLine
]

{ #category : #'stream out' }
SocketStream >> nextPut: char [
	"Put a single Character or byte onto the stream."

	| toPut |
	toPut := binary ifTrue: [char asInteger] ifFalse: [char asCharacter].
	self adjustOutBuffer: 1.
	outBuffer at: outNextToWrite put: toPut.
	outNextToWrite := outNextToWrite + 1.
	self checkFlush.
	"return the argument - added by kwl"
	^ char
]

{ #category : #'stream out' }
SocketStream >> nextPutAll: aCollection [
	"Put a String or a ByteArray onto the stream.
	Currently a large collection will allocate a large buffer."

	| toPut |
	toPut := binary ifTrue: [aCollection asByteArray] ifFalse: [aCollection asString].
	self adjustOutBuffer: toPut size.
	outBuffer replaceFrom: outNextToWrite to: outNextToWrite + toPut size - 1 with: toPut startingAt: 1.
	outNextToWrite := outNextToWrite + toPut size.
	self checkFlush.
	^aCollection
]

{ #category : #'stream out' }
SocketStream >> nextPutAllFlush: aCollection [
	"Put a String or a ByteArray onto the stream.
	You can use this if you have very large data - it avoids
	copying into the buffer (and avoids buffer growing)
	and also flushes any other pending data first."

	| toPut |
	toPut := binary ifTrue: [aCollection asByteArray] ifFalse: [aCollection asString].
	self flush. "first flush pending stuff, then directly send"
	socket isOtherEndClosed ifFalse: [
		[socket sendData: toPut count: toPut size]
			on: ConnectionTimedOut
			do: [:ex | shouldSignal ifFalse: ["swallow"]]]
]

{ #category : #configuration }
SocketStream >> noTimeout [
	"Do not use timeout."

	timeout := 0
]

{ #category : #configuration }
SocketStream >> outBufferSize [
	"Answers the current size of data in the outBuffer."

	^outNextToWrite - 1
]

{ #category : #'stream in' }
SocketStream >> peek [
	"Return next byte, if inBuffer is empty
	we recieve some more data and try again.
	Do not consume the byte."

	self atEnd ifTrue: [^nil].
	self isInBufferEmpty ifTrue:
		[self receiveData.
		self atEnd ifTrue: [^nil]].
	^inBuffer at: lastRead+1
]

{ #category : #'stream in' }
SocketStream >> peek: anInteger [
	"Answer anInteger bytes of data.
	Do not consume data.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!"

	| start |
	self receiveData: anInteger.
	start := lastRead + 1.
	^inBuffer copyFrom: start to: ((lastRead + anInteger) min: inNextToWrite - 1).
]

{ #category : #'stream in' }
SocketStream >> peekFor: aCharacterOrByte [
	"Read and return next character or byte
	if it is equal to the argument.
	Otherwise return false."

	| nextObject |
	self atEnd ifTrue: [^false].
	self isInBufferEmpty ifTrue: 
		[self receiveData.
		self atEnd ifTrue: [^false]].
	nextObject := inBuffer at: lastRead + 1.
	nextObject = aCharacterOrByte ifTrue: [
		lastRead := lastRead + 1.
		^true].
	^false

]

{ #category : #'stream in' }
SocketStream >> peekForAll: aString [
	"Answer whether or not the next string of characters in the receiver
	matches aString. If a match is made, advance over that string in the receiver and
	answer true. If no match, then leave the receiver alone and answer false.
	We use findString:startingAt: to avoid copying.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!"

	| sz start |
	sz := aString size.
	self receiveData: sz.
	(inNextToWrite - lastRead - 1) < sz ifTrue: [^false].
	start := lastRead + 1.
	(inBuffer findString: aString startingAt: start) = start
		ifFalse: [^false].
	lastRead := lastRead + sz.
	^true
]

{ #category : #private }
SocketStream >> position [
	^lastRead
]

{ #category : #printing }
SocketStream >> print: anObject [
	anObject printOn: self
]

{ #category : #printing }
SocketStream >> printOn: aStream [
	"Display buffer sizes."

	aStream nextPutAll: self class name.
	inBuffer ifNotNil: [
		aStream nextPutAll: '[inbuf:',
		(inBuffer size / 1024) rounded asString, 'kb/outbuf:',
		(outBuffer size / 1024) rounded asString, 'kb]']
]

{ #category : #'stream in' }
SocketStream >> readInto: aCollection startingAt: startIndex count: anInteger [
	"Read n objects into the given collection starting at startIndex. 
	Return number of elements that have been read."

	"Implementation note: This method DOES signal timeout if not 
	enough elements are received. It does NOT signal
	ConnectionClosed as closing the connection is the only way by
	which partial data can be read."

	| start amount |

	[self receiveData: anInteger] on: ConnectionClosed do:[:ex| ex return].

	"Inlined version of nextInBuffer: to avoid copying the contents"
	amount := anInteger min: (inNextToWrite - lastRead - 1).
	start := lastRead + 1.
	lastRead := lastRead + amount.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex + amount-1 
		with: inBuffer 
		startingAt: start.
	^amount
]

{ #category : #'private-socket' }
SocketStream >> receiveAvailableData [
	"Receive available data (as much as fits in the inBuffer)
	but not waiting for more to arrive.
	Return the position in the buffer where the
	new data starts, regardless if anything
	was read, see #adjustInBuffer."
	
	recentlyRead := socket receiveAvailableDataInto: inBuffer startingAt: inNextToWrite.
	^self adjustInBuffer: recentlyRead
]

{ #category : #'private-socket' }
SocketStream >> receiveData [
	"Receive data with timeout if it has been set.
	If shouldSignal is false we use the Socket methods
	that swallow those Exceptions, if it is true the
	caller will have to handle those Exceptions.
	Return the position in the buffer where the
	new data starts, regardless if anything
	was read, see #adjustInBuffer."
	
	recentlyRead := shouldSignal ifTrue: [
		self shouldTimeout ifTrue: [
				socket receiveDataSignallingTimeout: timeout
					into: inBuffer startingAt: inNextToWrite]
			ifFalse: [
				socket receiveDataSignallingClosedInto: inBuffer
					startingAt: inNextToWrite]]
				ifFalse: [
		self shouldTimeout ifTrue: [
			"This case is tricky, if it times out and is swallowed
			how does other methods calling this method repeatedly
			get to know that? And what should they do?"
				socket receiveDataTimeout: timeout
					into: inBuffer startingAt: inNextToWrite]
			ifFalse: [
				socket receiveDataInto: inBuffer
					startingAt: inNextToWrite]].
	^self adjustInBuffer: recentlyRead
]

{ #category : #control }
SocketStream >> receiveData: nBytes [
	"Keep reading the socket until we have nBytes
	in the inBuffer or we reach the end. This method
	does not return data, but can be used to make sure
	data has been read into the buffer from the Socket
	before actually reading it from the FastSocketStream.
	Mainly used internally. We could also adjust the buffer
	to the expected amount of data and avoiding several
	incremental grow operations.

	NOTE: This method doesn't honor timeouts if shouldSignal
	is false! And frankly, I am not sure how to handle that
	case or if I care - I think we should always signal."

	[self isConnected and: [nBytes > self inBufferSize]]
		whileTrue: [self receiveData]
]

{ #category : #'private-socket' }
SocketStream >> receiveDataIfAvailable [
	"Only used to check that there really is data to read
	from the socket after it signals dataAvailable.
	It has been known to signal true and then still
	not have anything to read. See also isDataAvailable.
	Return the position in the buffer where the
	new data starts, regardless if anything
	was read, see #adjustInBuffer."

	recentlyRead := socket receiveSomeDataInto: inBuffer startingAt: inNextToWrite.
	^self adjustInBuffer: recentlyRead
]

{ #category : #control }
SocketStream >> recentlyRead [
	"Return the number of bytes read
	during the last socket operation."
	
	^recentlyRead
]

{ #category : #private }
SocketStream >> resetBuffers [
	"Recreate the buffers with default start sizes."

	inBuffer := self streamBuffer: bufferSize.
	lastRead := 0.
	inNextToWrite := 1.
	outBuffer := self streamBuffer: bufferSize.
	outNextToWrite := 1
]

{ #category : #private }
SocketStream >> resizeInBuffer: newSize [
	"Resize the inBuffer by recreating it.
	This also has the effect of getting rid of
	dead data above inNextToWrite.
	<newSize> must >= inBuffer size!"

	inBuffer := (self streamBuffer: newSize)
					replaceFrom: 1 to: inNextToWrite - 1 with: inBuffer startingAt: 1
]

{ #category : #'stream out' }
SocketStream >> sendCommand: aString [
	"Sends a String ending it with CR LF and then flush
	causing it to block until sent."

	self nextPutAll: aString, String crlf; flush
]

{ #category : #configuration }
SocketStream >> shouldSignal [
	"If shouldSignal is enabled the Socket Exceptions
	ConnectionClosed and ConnectionTimedOut
	will not be swallowed. Default is true.
	For more info, see #shouldSignal:"

	^shouldSignal
]

{ #category : #configuration }
SocketStream >> shouldSignal: aBoolean [
	"If shouldSignal is enabled the Socket Exceptions
	ConnectionClosed and ConnectionTimedOut will not be swallowed.
	Default is true. And please - don't set it to false - it is better to
	use an exception handler (see below)  and several methods
	in this class will not honour timeouts (says so in their method comments).
	Also, it is quite hard to understand what for example #upToEnd
	should return to indicate a timeout.
	
	Wrap your use of SocketStream with a handler like:
	
	[stuff := mySocketStream next: 10]
		on: ConnectionClosed, ConnectionTimedOut
		do: [:ex |
			Transcript show: 'Oops! Did not get my ten bytes!;cr]
	"

	shouldSignal := aBoolean
]

{ #category : #testing }
SocketStream >> shouldTimeout [
	^self timeout > 0
]

{ #category : #'stream in' }
SocketStream >> skip: anInteger [
	"Skip a number of bytes.
	This is faster than #next: since it does not
	have to copy and return a new String or ByteArray.

	NOTE: This method doesn't honor timeouts if shouldSignal is false!"

	self receiveData: anInteger.
	lastRead := (lastRead + anInteger) min: inNextToWrite - 1
]

{ #category : #configuration }
SocketStream >> socket [
	^socket
]

{ #category : #configuration }
SocketStream >> socket: aSocket [
	socket := aSocket
]

{ #category : #'stream out' }
SocketStream >> space [
	self nextPut: Character space
]

{ #category : #private }
SocketStream >> streamBuffer: size [
	"Create a buffer of the correct class and given size."

	^(self isBinary
		ifTrue: [ByteArray]
		ifFalse: [String]) new: size
]

{ #category : #configuration }
SocketStream >> timeout [
	"Lazily initialized unless it has been set explicitly."

	timeout ifNil: [timeout := Socket standardTimeout].
	^timeout
]

{ #category : #configuration }
SocketStream >> timeout: seconds [
	timeout := seconds
]

{ #category : #'stream in' }
SocketStream >> upTo: aCharacterOrByte [
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of anObject in the receiver. If  anObject is not in the collection, answer the entire rest of the receiver."

	"Note: The 100k limit below is compatible with the previous version though arguably incorrect. If you need unbounded behavior either up the argument or provide nil in which case we'll read until we get it or run out of memory"

	^ binary 
		ifTrue: [self upTo: aCharacterOrByte asInteger limit: 100000]
		ifFalse: [ self upTo: aCharacterOrByte asCharacter limit: 100000]
]

{ #category : #'stream in' }
SocketStream >> upTo: aCharacterOrByte limit: nBytes [
	"Return data up to, but not including given character or byte. If the character is not in the stream, or not found within nBytes answer the available contents of the stream"

	| index result searchedSoFar |
	"Look in the current inBuffer first"
	index := inBuffer indexOf: aCharacterOrByte startingAt: lastRead + 1.

	(index > 0 and: [(index + 1) <= inNextToWrite]) ifTrue: ["found it"
		result := self nextInBuffer: index - lastRead - 1.
		self skip: 1.
		^ result
	].

	[searchedSoFar :=  self inBufferSize.
	"Receive more data"
	self receiveData.
	"We only get recentlyRead = 0 in the case of a non-signaling socket close."
	recentlyRead > 0] whileTrue:[
		"Data begins at lastRead + 1, we add searchedSoFar as offset."

		index := inBuffer indexOf: aCharacterOrByte
						startingAt: (lastRead + searchedSoFar + 1).
		(index > 0 and: [(index + 1) <= inNextToWrite]) ifTrue: ["found it"
			result := self nextInBuffer: index - lastRead - 1.
			self skip: 1.
			^ result
		].

		"Check if we've exceeded the max. amount"
		(nBytes notNil and:[inNextToWrite - lastRead > nBytes]) 
			ifTrue:[^self nextAllInBuffer].
	].

	"Windows doesn't raise an Exception when connection is closed at first, so we check here the status of the connection"
	self shouldSignal ifTrue: [
		self isOtherEndConnected ifFalse: [ConnectionClosed signal: 'Connection closed while waiting for data.'].
	].

	"not found and (non-signaling) connection was closed"
	^self nextAllInBuffer
]

{ #category : #'stream in' }
SocketStream >> upToAll: aStringOrByteArray [
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	"Note: The 100k limit below is compatible with the previous version though arguably incorrect. If you need unbounded behavior either up the argument or provide nil in which case we'll read until we get it or run out of memory"

	^self upToAll: aStringOrByteArray limit: 100000
]

{ #category : #'stream in' }
SocketStream >> upToAll: aStringOrByteArray limit: nBytes [
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aStringOrByteArray. If aCollection is not in the stream, or not found within nBytes answer the available contents of the stream"

	| index sz result searchedSoFar |
	sz := aStringOrByteArray size.
	"Look in the current inBuffer first"
	index := inBuffer indexOfSubCollection: aStringOrByteArray
						startingAt: lastRead - sz + 2.
	(index > 0 and: [(index + sz) <= inNextToWrite]) ifTrue: ["found it"
		result := self nextInBuffer: index - lastRead - 1.
		self skip: sz.
		^ result
	].

	[searchedSoFar :=  self inBufferSize.
	"Receive more data"
	self receiveData.
	recentlyRead > 0] whileTrue:[

		"Data begins at lastRead + 1, we add searchedSoFar as offset and 
		backs up sz - 1 so that we can catch any borderline hits."

		index := inBuffer indexOfSubCollection: aStringOrByteArray
						startingAt: (lastRead + searchedSoFar - sz + 2 max: 1).
		(index > 0 and: [(index + sz) <= inNextToWrite]) ifTrue: ["found it"
			result := self nextInBuffer: index - lastRead - 1.
			self skip: sz.
			^ result
		].
		"Check if we've exceeded the max. amount"
		(nBytes notNil and:[inNextToWrite - lastRead > nBytes]) 
			ifTrue:[^self nextAllInBuffer].
	].

	"Windows doesn't raise an Exception when connection is closed at first, so we check here the status of the connection"
	self shouldSignal ifTrue: [
		self isOtherEndConnected ifFalse: [ConnectionClosed signal: 'Connection closed while waiting for data.'].
	].

	"not found and (non-signaling) connection was closed"
	^self nextAllInBuffer
]

{ #category : #'stream in' }
SocketStream >> upToEnd [
	"Answer all data coming in on the socket until the socket
	is closed by the other end, or we get a timeout.
	This means this method catches ConnectionClosed by itself.
	
	NOTE: Does not honour timeouts if shouldSignal is false!"

	[[self isConnected] whileTrue: [self receiveData]]
		on: ConnectionClosed
		do: [:ex | "swallow it"]. 
	^self nextAllInBuffer
]
