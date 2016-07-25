accessing
infoAt: key ifAbsentPut: putBlock
	info ifNil: [ info := Dictionary new ].
	^ info at: key ifAbsentPut: putBlock