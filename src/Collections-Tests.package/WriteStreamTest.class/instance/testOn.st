tests
testOn
	| collection stream |
	collection := String new.
	stream := self streamClass on: collection.
	self
		assert: stream originalContents == collection;
		assert: stream position isZero;
		assert: stream contents isEmpty;
		assert: stream size isZero