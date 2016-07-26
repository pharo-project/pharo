matching
replacingBlock
	^ valueBlock ifNil: [valueBlock := self createReplacingBlock]