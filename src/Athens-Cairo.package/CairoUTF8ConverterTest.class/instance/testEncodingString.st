tests
testEncodingString
	|  pos |

	
	pos := encoder convert: 'abc' from: 1 to: 3.
	
	self assert: pos equals: 3.
	self assert: (encoder buffer first:3)   equals: #[97 98 99]
