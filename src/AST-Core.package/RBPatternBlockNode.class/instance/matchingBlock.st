matching
matchingBlock
	^ valueBlock ifNil: [valueBlock := self createMatchingBlock]