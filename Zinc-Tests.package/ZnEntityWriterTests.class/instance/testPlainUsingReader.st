testing
testPlainUsingReader
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