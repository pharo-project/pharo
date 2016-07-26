private
compileSelector: selector returnValue: aLiteral classified: aProtocolName
	| source |
	source := String streamContents: [ :stream | 
		stream 
			nextPutAll: selector asString;
			nextPut: Character cr;
			nextPut: Character tab;
			nextPutAll: '^ ';
			nextPutAll: aLiteral ].
	manifestClass class
		compile: source
		classified: aProtocolName