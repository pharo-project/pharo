testing
testReadingApplicationFormUrlEncodingNoLength
	| input entity |
	input := #(111 97 117 116 104 95 116 111 107 101 110 95 115 101 99 114 101 116 61 98 121 112 104 57 99 109 104 106 49 53 101 108 121 56 38 111 97 117 116 104 95 116 111 107 101 110 61 56 54 101 112 106 51 116 118 100 49 107 115 120 111 57)
		asByteArray.
	entity := ZnEntity readFrom: input readStream usingType: ZnMimeType applicationFormUrlEncoded andLength: nil.
	self deny: entity isEmpty.
	self assert: (entity includesField: 'oauth_token_secret').
	self assert: (entity includesField: 'oauth_token').
	self assert: (entity at: 'oauth_token_secret') equals: 'byph9cmhj15ely8'.
	self assert: (entity at: 'oauth_token') equals: '86epj3tvd1ksxo9'