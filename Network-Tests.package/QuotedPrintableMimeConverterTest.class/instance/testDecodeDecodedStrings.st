testing
testDecodeDecodedStrings

	self 
		assert: (self decode: 'Hätten Hüte ein ß im Namen, wären sie möglicherweise keine Hüte mehr')
		equals: 'Hätten Hüte ein ß im Namen, wären sie möglicherweise keine Hüte mehr'