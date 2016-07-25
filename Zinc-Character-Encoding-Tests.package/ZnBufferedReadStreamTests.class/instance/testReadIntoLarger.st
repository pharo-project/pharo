testing
testReadIntoLarger
	| stream buffer count |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 4.
	buffer := String new: 10.
	count := stream readInto: buffer startingAt: 1 count: 10.
	self assert: count equals: 10.
	self assert: buffer equals: '0123456789'	 