enumerating
entriesDo: aBlockClosure 
	"Evaluate the closure on each entry"

	self fileReference exists ifTrue: [
		self readEntriesWith: [:readStream | 
			[ readStream atEnd ] whileFalse: [
				(self entryReader nextEntryFrom: readStream) in: aBlockClosure ] ] ].

	"Finally, the entries still not written"
	self entryBufferDo: [:entryAndLocalName | aBlockClosure value: entryAndLocalName key ].
