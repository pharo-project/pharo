testing
hasSelfReferences
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: 'self' do: [:aNode :answer | true].
	self hasOnlySelfReturns 
		ifTrue: [searcher matches: '^self' do: [:aNode :answer | answer]].
	^searcher executeTree: parseTree initialAnswer: false