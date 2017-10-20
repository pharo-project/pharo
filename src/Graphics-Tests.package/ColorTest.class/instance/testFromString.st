tests
testFromString
	self assert: ((Color fromString: '#FF8800') asHexString sameAs: 'ff8800');
		assert: ((Color fromString: 'FF8800') asHexString sameAs: 'ff8800');
		assert: ((Color fromString: 'white') asHexString sameAs: 'ffffff');
		assert: ((Color fromString: 'black') asHexString sameAs: '000000');
		assert: ((Color fromString: nil) asHexString sameAs: 'ffffff');
		assert: ((Color fromString: 'inexistent color') asHexString sameAs: 'ffffff'); "should return white"
		assert: ((Color fromString: 'XXXXXX') asHexString sameAs: 'ffffff'); "not alphanumeric"
		assert: ((Color fromString: '00000000') asHexString sameAs: 'ffffff'). "too many digits"
		
	self
		assert: (Color fromString: 'DARKGRAY') = Color darkGray description: 'Color can be specified with a case insensitive color name';
		assert: (Color fromString: '#blue') = Color blue description: 'Color can be specified with a leading literal sharp'.