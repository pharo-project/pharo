testing
testGetGeoIP
	| result client |
	result := (client := ZnClient new)
		systemPolicy;
		url: self t3EasyGeoIPUrl;
		queryAt: 'address' put: '81.83.7.35';
		accept: ZnMimeType applicationJson;
		contentReader: [ :entity | 
					Smalltalk globals
						at: #NeoJSONReader
						ifPresent: [ :parserClass | parserClass fromString: entity contents ]
						ifAbsent: [ ^ self ] ];
		ifFail: [ ^ self fail ];
		get.
	self assert: result isDictionary.
	self assert: (result at: #country) = 'BE'.
	client close