tests - printing
testAsStringWithCommas
	self assert: 123456789 asStringWithCommas equals: '123,456,789'.
	self assert: -123456789 asStringWithCommas equals: '-123,456,789'