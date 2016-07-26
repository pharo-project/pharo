tests
testNextIndexPut

	| indexStream targetStream |
	targetStream := #() writeStream.
	indexStream := self 
		indexStreamOn: targetStream
		digits: 1.
	
	indexStream nextIndexPut: 0.
	indexStream nextIndexPut: 1.