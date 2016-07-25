testing
testPeek
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 8.
	'0123456789' do: [ :each |
		self deny: stream atEnd.
		self assert: stream peek equals: each.
		self assert: stream next equals: each ].
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil