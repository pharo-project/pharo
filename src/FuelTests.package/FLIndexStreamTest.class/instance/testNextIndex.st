tests
testNextIndex

	| indexes indexStream targetStream |
	indexes := #(0 1 128 255).
	targetStream := #() writeStream.

	indexStream := self 
		indexStreamOn: targetStream
		digits: 1.
	indexes do: [:x | indexStream nextIndexPut: x ].

	indexStream := self 
		indexStreamOn: targetStream contents readStream
		digits: 1.

	indexes do: [:x | self assert: indexStream nextIndex = x ].