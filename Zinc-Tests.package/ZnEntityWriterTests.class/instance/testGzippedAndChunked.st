testing
testGzippedAndChunked
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