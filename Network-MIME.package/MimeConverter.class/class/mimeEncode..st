convenience
mimeEncode: aCollectionOrStream
	^ String streamContents: [:out |
		self mimeEncode: aCollectionOrStream to: out]