testing
testBuffering
	| stream |
	stream := ZnBufferedReadStream on: '01234567890123456789' readStream.
	stream sizeBuffer: 8.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self assert: stream atEnd	