tests
testFallbackReplaceFromToWithForWideString
	| testString byteArray stringByteSize |
	testString := 'Test string' asWideString.
	stringByteSize := 'Test string' byteSize.
	byteArray := ByteArray new: stringByteSize.
	
	self
		shouldnt: [
			byteArray
				replaceFrom: 1
				to: stringByteSize
				with: testString
				startingAt: 1 ]
		raise: Exception
		description: 'Primitive 105 should be optional for ByteArray'
