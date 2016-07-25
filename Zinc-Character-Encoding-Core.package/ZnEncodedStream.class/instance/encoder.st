accessing
encoder
	^ encoder ifNil: [ encoder := self class defaultEncoder ]