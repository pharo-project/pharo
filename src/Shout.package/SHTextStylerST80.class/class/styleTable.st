style table
styleTable
	^ styleTable
		ifNil: [styleTable := self defaultStyleTable]