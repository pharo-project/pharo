huffman trees
mapValues: values by: valueMap
	| oldValue |
	valueMap ifNil: [^values].
	1 to: values size do:[:i|
		oldValue := values at: i.
		"Note: there may be nil values if not all values are used"
		oldValue 
			ifNil: [^values]
			ifNotNil: [values at: i put: (valueMap at: oldValue+1)]].
