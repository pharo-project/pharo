running
testJoinUsingLastOnArrayOfStrings
	self
		assert: (#('Pharo is modern' 'open source' 'highly portable' 'fast' 'full-featured' ) joinUsing: ', ' last: ' and ')
		equals: 'Pharo is modern, open source, highly portable, fast and full-featured'