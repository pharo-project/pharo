tests
testSecondsRoundTrip
	| now now2 |
	now := DateAndTime fromSeconds: 0.
	now2 := DateAndTime fromSeconds: now asSeconds.
	self assert: now equals: now2.