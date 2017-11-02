"
I am ZdcSecureSocketStream, a binary read/write stream for SSL communication.

I am a ZdcOptimizedSocketStream.

When I am used as a client, call #connect on me before using me as a normal stream.

When I am used as a server, call #accept on me before using me as a normal stream.

Currently, certificate management is ignored.

"
Class {
	#name : #ZdcSecureSocketStream,
	#superclass : #ZdcOptimizedSocketStream,
	#instVars : [
		'sslSession',
		'in',
		'out',
		'connecting'
	],
	#category : 'Zodiac-Core'
}

{ #category : #ssl }
ZdcSecureSocketStream >> accept [
	"Do the SSL server handshake."
	
	| count result |
	self resetEncryptedBuffers.
	count := 0.
	connecting := true.
	[ self sslSession isConnected ] whileFalse: [
		count := self readEncryptedBytes: in startingAt: 1 count: in size.
		result := self sslSession accept: in from: 1 to: count into: out.
		result < -1 ifTrue: [ 
			^ self sslException: 'accept failed' code: result ].
		result > 0 ifTrue: [ 
			self flushEncryptedBytes: out startingAt: 1 count: result ] ].
	connecting := false
]

{ #category : #testing }
ZdcSecureSocketStream >> atEnd [
	"I am atEnd when there is no more data to be read and there never will be.
	This means that readBuffer must be empty, there must be no more unread data 
	available at the socket, and the socket must be closed"
	
	readBuffer isEmpty ifFalse: [ ^ false ].
	socket notNil
		ifTrue: [
			"Try reading (there might stil be data in the SSL session) and test again"
			[ self fillReadBufferNoWait ]
				on: ConnectionClosed 
				do: [ ^ true ].
			readBuffer isEmpty ifFalse: [ ^ false ] ].   
	^ self isConnected not
]

{ #category : #initialize-release }
ZdcSecureSocketStream >> close [
	"Close the stream, flush if necessary. 
	Destory the SSLSession object."

	[ super close ] ensure: [ 
		sslSession ifNotNil: [
			sslSession destroy.
			sslSession := nil ].
		ZdcByteArrayManager current recycle: in; recycle: out.
		in := out := nil ]
]

{ #category : #ssl }
ZdcSecureSocketStream >> connect [
	"Do the SSL client handshake."
	
	| count result |
	self resetEncryptedBuffers.
	count := 0.
	connecting := true.
	[ (result := self sslSession connect: in from: 1 to: count into: out) = 0 ] whileFalse: [
		result < -1 ifTrue: [ 
			^ self sslException: 'connect failed' code: result ].
		result > 0 ifTrue: [ 
			self flushEncryptedBytes: out startingAt: 1 count: result ].
		count := self readEncryptedBytes: in startingAt: 1 count: in size ].
	connecting := false
	
]

{ #category : #'private in' }
ZdcSecureSocketStream >> fillBytes: bytes startingAt: offset count: count [
	"Ask to read count elements into bytes starting at offset. Do not wait. Return read count.
	Overwritten: decrypt and if necessary ask the socket for encrypted data using a super call."
	
	| processedCount inCount |
	self resetEncryptedBuffers.
	"The SSL session might contain left over state, get it"
	processedCount := self sslSession decrypt: in from: 1 to: 0 into: out.	
	"We explicitly ignore possible errors here"
	processedCount > 0
		ifTrue: [
			bytes replaceFrom: offset to: offset + processedCount - 1 with: out startingAt: 1.
			^ processedCount ].
	"Now, get new encrypted bytes and decrypt if needed"
	inCount := self fillEncryptedBytes: in startingAt: 1 count: in size.
	inCount > 0
		ifTrue: [ 
			processedCount := self sslSession decrypt: in from: 1 to: inCount into: out.
			"We explicitly ignore possible errors here"
			processedCount > 0
				ifTrue: [
					bytes replaceFrom: offset to: offset + processedCount - 1 with: out startingAt: 1.
					^ processedCount ] ].
	^ 0
]

{ #category : #'private in' }
ZdcSecureSocketStream >> fillEncryptedBytes: bytes startingAt: offset count: count [
	"Read encrypted bytes from the network. Do not wait."
	
	^ super fillBytes: bytes startingAt: offset count: count
]

{ #category : #'private out' }
ZdcSecureSocketStream >> flushBytes: bytes startingAt: offset count: count [
	"Ask to write count bytes starting from offset. Wait. Fail if not successful
	Overwritten: first encrypt the data, then send it to the socket using a super call."
	
	| remaining currentOffset |
	self isConnected ifFalse: [ ConnectionClosed signal: 'Cannot write data' ].  
	remaining := count.
	currentOffset := offset.
	[ remaining > 0 ] whileTrue: [ | chunkCount writeCount |
		self resetEncryptedBuffers.
		chunkCount := 4096 min: remaining.
		writeCount := self sslSession encrypt: bytes from: currentOffset to: currentOffset + chunkCount - 1 into: out.
		writeCount < 0 ifTrue: [ ^ self sslException: 'encrypt failed' code: writeCount ].
		self flushEncryptedBytes: out startingAt: 1 count: writeCount.
		remaining := remaining - chunkCount.
		currentOffset := currentOffset + chunkCount ]
]

{ #category : #'private out' }
ZdcSecureSocketStream >> flushEncryptedBytes: bytes startingAt: offset count: count [
	"Write encrypted bytes to the network."
	
	^ super flushBytes: bytes startingAt: offset count: count
]

{ #category : #initialize-release }
ZdcSecureSocketStream >> initialize [ 
	super initialize.
	connecting := false
]

{ #category : #initialize-release }
ZdcSecureSocketStream >> initializeBuffers [
	"The maximum payload message length of a TLS record is 16Kb,
	add a margin for the header and trailer."
	
	readBuffer := ZdcIOBuffer onByteArrayOfSize: 16 * 1024.
	writeBuffer := ZdcIOBuffer onByteArrayOfSize: 4 * 1024.
	in := ZdcByteArrayManager current byteArrayOfSize: 4096 zero: false.
	out := ZdcByteArrayManager current byteArrayOfSize: (16 + 1) * 1024 zero: false

]

{ #category : #testing }
ZdcSecureSocketStream >> isConnected [
	"Are we connected at the socket level ?
	Has the SSL handshake been done successfully ?"

	^ super isConnected 
		and: [ sslSession notNil and: [ connecting or: [ sslSession isConnected ] ] ]
]

{ #category : #testing }
ZdcSecureSocketStream >> isDataAvailable [
	"Return true when there is data available for reading.
	This does not block."
	
	readBuffer isEmpty ifFalse: [ ^ true ].
	socket notNil
		ifTrue: [
			"Try reading (there might stil be data in the SSL session) and test again"
			[ self fillReadBufferNoWait ]
				on: ConnectionClosed 
				do: [ ^ false ].
			readBuffer isEmpty ifFalse: [ ^ true ] ].   
	^ false
]

{ #category : #'private in' }
ZdcSecureSocketStream >> readEncryptedBytes: bytes startingAt: offset count: count [
	"Read encrypted bytes from the network. Wait if necessary."
	
	| result |
	result := super fillBytes: bytes startingAt: offset count: count.
	^ result = 0
		ifTrue: [
			self socketWaitForData. "when successful, recurse, else signal exception"
			self readEncryptedBytes: bytes startingAt: offset count: count ]
		ifFalse: [
			result ]
]

{ #category : #private }
ZdcSecureSocketStream >> resetEncryptedBuffers [
	"Not necessary, but useful for debugging"
	
	in atAllPut: 0.
	out atAllPut: 0
]

{ #category : #private }
ZdcSecureSocketStream >> sslException: text code: code [
	self error: 
		(String streamContents: [ :stream |
			stream << 'SSL Exception: ' << text << ' [code:'.
			stream print: code.
			stream << ']' ]) 
]

{ #category : #accessing }
ZdcSecureSocketStream >> sslSession [
	"Return the underlying ZdcAbstractSSLSession object 
	that we use for encryption and decryption, 
	as well as connection setup handshaking, connect and accept"
	
	sslSession isNil ifTrue: [ sslSession := self sslSessionClass new ].
	^ sslSession
]

{ #category : #accessing }
ZdcSecureSocketStream >> sslSessionClass [
	^ ZdcPluginSSLSession 
]
