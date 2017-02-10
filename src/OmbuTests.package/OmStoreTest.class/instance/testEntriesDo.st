tests
testEntriesDo
	1 to: 7 do: [ :each | store newEntry: (OmEntry content: each) ].

	self beforeAndAfterFlushStore: [
		| contents |
		contents := Array streamContents: [ :stream | 
			store entriesDo: [ :entry | stream nextPut: entry content ] ].

		self assert: contents equals: (1 to: 7) asArray ]