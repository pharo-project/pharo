accessing
hACTable
	hACTable ifNil: [ hACTable := Array new: HuffmanTableSize ].
	^ hACTable