enumerating
entriesDo: aBlockClosure 
	"Evaluate the closure on each entry"

	self fileReference exists ifTrue: [
		self readEntriesWith: [:readStream | 
			| reader |
			reader := self newEntryReader.
			reader stream: readStream.
			[ readStream atEnd ] whileFalse: [
				aBlockClosure value: reader nextEntry ] ] ].

	"Finally, the entries still not written"
	self entryBufferDo: [:entryAndLocalName | 
		aBlockClosure value: entryAndLocalName key ].
