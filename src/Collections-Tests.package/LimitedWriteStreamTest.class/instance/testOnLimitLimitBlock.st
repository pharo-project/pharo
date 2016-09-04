tests
testOnLimitLimitBlock
	| collection overLimit limitBlock stream |
	collection := String new.
	overLimit := false.
	limitBlock := [ overLimit := true ].
	stream := self streamClass
		on: collection
		limit: self stringSize
		limitBlock: limitBlock.
	self
		assert: stream originalContents == collection;
		assert: stream position isZero;
		assert: stream limit = self stringSize;
		assert: stream limitBlock = limitBlock;
		deny: overLimit