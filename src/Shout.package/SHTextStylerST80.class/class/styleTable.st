accessing
styleTable
	^ styleTable
		ifNil: [styleTable := self defaultStyleTable]