"
I am ZnMessageBenchmark helps to test the benchmarking and profiling of ZnMessage writing and reading.

Instance Variables
	buffer:					<ByteArray>
	message:				<ZnObject>
	representation:		<ByteArray>

ZnMessageBenchmark new
	simpleRequest;
	write: 10000.

ZnMessageBenchmark new
	simpleRequest;
	writeRepresentation;
	read: 10000.

ZnMessageBenchmark new
	simpleResponse;
	write: 10000.

ZnMessageBenchmark new
	simpleResponse;
	writeRepresentation;
	read: 10000.

"
Class {
	#name : #ZnMessageBenchmark,
	#superclass : #Object,
	#instVars : [
		'message',
		'representation',
		'buffer'
	],
	#category : 'Zinc-Tests'
}

{ #category : #accessing }
ZnMessageBenchmark class >> bench: messages [
	| results |
	results := Dictionary new.
	messages 
		do: [ :each | | bench report |
			bench := self new.
			bench perform: each.
			bench writeRepresentation.
			report := 'Writing {1} - Reading {2}' format: { bench benchWrite. bench benchRead }.
			results at: each put: report ] 
		displayingProgress: 'Benchmarking...'.
	^ results
]

{ #category : #accessing }
ZnMessageBenchmark class >> benchAll [
	^ self bench: self messages
]

{ #category : #accessing }
ZnMessageBenchmark class >> messages [
	^ self requests , self responses
]

{ #category : #accessing }
ZnMessageBenchmark class >> requests [
	^ #( simpleRequest standardRequest postRequest )
]

{ #category : #accessing }
ZnMessageBenchmark class >> responses [
	^ #( 
		simpleResponse 
		textResponse8k textResponse64k textResponse256k
		textResponseWide8k textResponseWide64k textResponseWide256k
		asciiResponse8k asciiResponse64k asciiResponse256k
		binaryResponse8k binaryResponse64k binaryResponse256k )
]

{ #category : #initialize-release }
ZnMessageBenchmark >> asciiResponse256k [
	message := ZnResponse ok: 
		(ZnEntity 
			with: (self randomAsciiString: 256 * 1024) 
			type: 'text/plain;charset=ascii')
]

{ #category : #initialize-release }
ZnMessageBenchmark >> asciiResponse64k [
	message := ZnResponse ok: 
		(ZnEntity 
			with: (self randomAsciiString: 64 * 1024) 
			type: 'text/plain;charset=ascii')
]

{ #category : #initialize-release }
ZnMessageBenchmark >> asciiResponse8k [
	message := ZnResponse ok: 
		(ZnEntity 
			with: (self randomAsciiString: 8 * 1024) 
			type: 'text/plain;charset=ascii')
]

{ #category : #accessing }
ZnMessageBenchmark >> benchRead [
	^ [ self read ] bench
]

{ #category : #accessing }
ZnMessageBenchmark >> benchWrite [
	^ [ self write ] bench
]

{ #category : #initialize-release }
ZnMessageBenchmark >> binaryResponse256k [
	message := ZnResponse ok: (ZnEntity with: (self randomBytes: 256 * 1024))
]

{ #category : #initialize-release }
ZnMessageBenchmark >> binaryResponse64k [
	message := ZnResponse ok: (ZnEntity with: (self randomBytes: 64 * 1024))
]

{ #category : #initialize-release }
ZnMessageBenchmark >> binaryResponse8k [
	message := ZnResponse ok: (ZnEntity with: (self randomBytes: 8 * 1024))
]

{ #category : #accessing }
ZnMessageBenchmark >> buffer [
	^ buffer ifNil: [  buffer := ByteArray new: ZnUtils streamingBufferSize ]
]

{ #category : #accessing }
ZnMessageBenchmark >> message [
	^ message
]

{ #category : #initialize-release }
ZnMessageBenchmark >> postRequest [
	message := (ZnRequest post: 'http://zn.stfx.eu/echo/one/two/three?param1=123&param2=foobar')
		setAcceptEncodingGzip;
		setBasicAuthenticationUsername: 'john' password: 'secret';
		entity: (ZnEntity with: (self randomBytes: 512));
		yourself
]

{ #category : #private }
ZnMessageBenchmark >> randomAsciiString: count [
	^ String
		new: count
		streamContents: [ :stream | 
			count timesRepeat: [ stream nextPut: '0123456789ABCDEF' atRandom ] ]
]

{ #category : #private }
ZnMessageBenchmark >> randomBytes: count [
	| bytes |
	bytes := ByteArray new: count.
	1 to: count do: [ :each | 
		bytes at: each put: (#(65 66 67 68) at: each \\ 4 + 1) ].
	^ bytes
]

{ #category : #private }
ZnMessageBenchmark >> randomUnicodeString: count [
	^ String
		new: count
		streamContents: [ :stream | 
			"This is about 6% non-ASCII characters, still a ByteString"
			count timesRepeat: [ stream nextPut: '0123456789ABCDEF-élève en Français-0123456789ABCDEF' atRandom ] ]
]

{ #category : #private }
ZnMessageBenchmark >> randomUnicodeWideString: count [
	| alphabet |
	"This is about 6% non-ASCII characters, 6% non-octet characters, always a WideString"
	alphabet := '0123456789ABCDEF-élève en Français-0123456789ABCDEF', 
		(WideString withAll: (#(300 400 500) collect: [ :each | each asCharacter ])).
	^ WideString
		new: count
		streamContents: [ :stream | 
			count timesRepeat: [ stream nextPut: alphabet atRandom ] ]
]

{ #category : #accessing }
ZnMessageBenchmark >> read [
	| readStream |
	readStream := representation readStream.
	^ message class readFrom: readStream
]

{ #category : #accessing }
ZnMessageBenchmark >> read: count [
	count timesRepeat: [ self read ]
]

{ #category : #initialize-release }
ZnMessageBenchmark >> simpleRequest [
	message := ZnRequest get: 'http://zn.stfx.eu/dw-bench'
]

{ #category : #initialize-release }
ZnMessageBenchmark >> simpleResponse [
	message := ZnResponse ok: (ZnEntity html: ZnDefaultServerDelegate new generateDWBench)
]

{ #category : #private }
ZnMessageBenchmark >> sizeBuffer: size [
	buffer := ByteArray new: size
]

{ #category : #initialize-release }
ZnMessageBenchmark >> standardRequest [
	message := (ZnRequest get: 'http://zn.stfx.eu/echo/one/two/three?param1=123&param2=foobar')
		setAcceptEncodingGzip;
		setBasicAuthenticationUsername: 'john' password: 'secret';
		yourself
]

{ #category : #initialize-release }
ZnMessageBenchmark >> textResponse256k [
	message := ZnResponse ok: (ZnEntity with: (self randomUnicodeString: 256 * 1024))
]

{ #category : #initialize-release }
ZnMessageBenchmark >> textResponse64k [
	message := ZnResponse ok: (ZnEntity with: (self randomUnicodeString: 64 * 1024))
]

{ #category : #initialize-release }
ZnMessageBenchmark >> textResponse8k [
	message := ZnResponse ok: (ZnEntity with: (self randomUnicodeString: 8 * 1024))
]

{ #category : #initialize-release }
ZnMessageBenchmark >> textResponseWide256k [
	message := ZnResponse ok: (ZnEntity with: (self randomUnicodeWideString: 256 * 1024))
]

{ #category : #initialize-release }
ZnMessageBenchmark >> textResponseWide64k [
	message := ZnResponse ok: (ZnEntity with: (self randomUnicodeWideString: 64 * 1024))
]

{ #category : #initialize-release }
ZnMessageBenchmark >> textResponseWide8k [
	message := ZnResponse ok: (ZnEntity with: (self randomUnicodeWideString: 8 * 1024))
]

{ #category : #accessing }
ZnMessageBenchmark >> write [
	| writeStream |
	writeStream := self buffer writeStream.
	message writeOn: writeStream.
	^ writeStream
]

{ #category : #accessing }
ZnMessageBenchmark >> write: count [
	count timesRepeat: [ self write ]
]

{ #category : #initialize-release }
ZnMessageBenchmark >> writeRepresentation [
	representation := self write contents.
	self sizeBuffer: representation size + 1024 
]

{ #category : #initialize-release }
ZnMessageBenchmark >> writeUsingGzipEncodingAndChunkingRepresentation [
	message
		setContentEncodingGzip;
		setTransferEncodingChunked.
	representation := self write contents.
	self sizeBuffer: representation size + 1024 
]
