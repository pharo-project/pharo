testing
testDecodePercent
	self assert: (ZnResourceMetaUtils decodePercent: 'foo%20bar') equals: 'foo bar'.
	self should: [ ZnResourceMetaUtils decodePercent: 'foo%%bar' ] raise: ZnCharacterEncodingError 