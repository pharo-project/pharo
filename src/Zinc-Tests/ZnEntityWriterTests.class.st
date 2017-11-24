Class {
	#name : #ZnEntityWriterTests,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnEntityWriterTests >> testChunkedOnly [
	| entity headers writer output reader result |
	entity := 	ZnEntity text: (String streamContents: [ :out |
		{ Collection. Stream. Exception. Magnitude } do: [ :cls |
			cls allSubclassesDo: [ :each | out nextPutAll: each comment ] ] ]).
	headers := ZnHeaders new
		acceptEntityDescription: entity;
		at: 'Transfer-Encoding' put: 'chunked';
		yourself.
	output := ByteArray streamContents: [ :out |
		writer := ZnEntityWriter new
			stream: out;
			headers: headers;
			yourself.
		writer writeEntity: entity ].
	reader := ZnEntityReader new
		stream: output readStream;
		headers: headers;
		yourself.
	result := reader readEntity.
	self assert: result contents equals: entity contents
]

{ #category : #testing }
ZnEntityWriterTests >> testGzippedAndChunked [
	| entity headers writer output reader result |
	entity := 	ZnEntity text: (String streamContents: [ :out |
		{ Collection. Stream. Exception. Magnitude } do: [ :cls |
			cls allSubclassesDo: [ :each | out nextPutAll: each comment ] ] ]).
	headers := ZnHeaders new
		acceptEntityDescription: entity;
		at: 'Content-Encoding' put: 'gzip';
		at: 'Transfer-Encoding' put: 'chunked';
		yourself.
	output := ByteArray streamContents: [ :out |
		writer := ZnEntityWriter new
			stream: out;
			headers: headers;
			yourself.
		writer writeEntity: entity ].
	reader := ZnEntityReader new
		stream: output readStream;
		headers: headers;
		yourself.
	result := reader readEntity.
	self assert: result contents equals: entity contents
]

{ #category : #testing }
ZnEntityWriterTests >> testPlain [
	| entity writer output |
	entity := ZnEntity text: 'Foo bar!'.
	output := ByteArray streamContents: [ :out |
		writer := ZnEntityWriter new
			stream: out;
			headers: ZnHeaders new;
			yourself.
		writer writeEntity: entity ].
	self assert: output equals: 'Foo bar!' asByteArray
]

{ #category : #testing }
ZnEntityWriterTests >> testPlainUsingReader [
	| entity headers writer output reader result |
	entity := ZnEntity text: 'Foo bar!'.
	headers := ZnHeaders new 
		acceptEntityDescription: entity; 
		yourself.
	output := ByteArray streamContents: [ :out |
		writer := ZnEntityWriter new
			stream: out;
			headers: headers;
			yourself.
		writer writeEntity: entity ].
	reader := ZnEntityReader new
		stream: output readStream;
		headers: headers;
		yourself.
	result := reader readEntity.
	self assert: result contents equals: 'Foo bar!'
]
