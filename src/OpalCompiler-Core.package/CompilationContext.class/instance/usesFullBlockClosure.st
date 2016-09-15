accessing
usesFullBlockClosure
	^ usesFullBlockClosure ifNil: [ usesFullBlockClosure := self class usesFullBlockClosure ].