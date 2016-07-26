macro transformations
transformBoolean: encoder
	^self
		checkBlock: (arguments at: 1)
		as: 'argument'
		from: encoder