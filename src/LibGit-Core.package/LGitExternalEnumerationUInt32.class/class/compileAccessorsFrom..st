class initialization
compileAccessorsFrom: aDictionary
	aDictionary keysAndValuesDo: [ :symbol :value |
		self class
			compile: (String streamContents: [ :stream |
				stream
					nextPutAll: symbol asString; cr; tab;
					nextPutAll: '^ self basicNew'; cr; tab; tab;
					nextPutAll: 'value: ';
					nextPutAll: value asString;
					nextPutAll: ';'; cr; tab; tab;
					nextPutAll: 'yourself' ])
			classified: 'accessing-values' ]