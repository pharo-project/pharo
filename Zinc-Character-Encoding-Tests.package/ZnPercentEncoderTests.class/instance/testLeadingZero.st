testing
testLeadingZero
	| encoder |
	encoder := ZnPercentEncoder new.
	self assert: (encoder encode: 'foo', Character tab asString, 'bar') equals: 'foo%09bar'.
	self assert: (encoder decode: 'foo%09bar') equals: 'foo', Character tab asString, 'bar'.
	self assert: (encoder encode: 'foo', Character lf asString, 'bar') equals: 'foo%0Abar'.
	self assert: (encoder decode: 'foo%0Abar') equals: 'foo', Character lf asString, 'bar'