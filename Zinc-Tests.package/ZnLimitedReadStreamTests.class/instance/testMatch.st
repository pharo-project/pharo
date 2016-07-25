testing
testMatch
	| stream |
	stream := ZnLimitedReadStream on: 'ababcabc' readStream limit: 8.
	self assert: (stream match: 'ab').
	self deny: stream atEnd.
	stream := ZnLimitedReadStream on: 'ababcabc' readStream limit: 8.
	self assert: (stream match: 'abc').
	self deny: stream atEnd.
	self assert: (stream match: 'abc').
	self assert: stream atEnd.
	stream := ZnLimitedReadStream on: 'ababcabc' readStream limit: 8.
	self deny: (stream match: 'xx').
	self assert: stream atEnd.
