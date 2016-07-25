transforming
hasOnlySelfReturns
	^hasOnlySelfReturns isNil 
		ifTrue: 
			[| searcher |
			searcher := RBParseTreeSearcher new.
			searcher
				matches: '^self' do: [:aNode :answer | answer];
				matches: '^`@object' do: [:aNode :answer | false].
			hasOnlySelfReturns := searcher executeTree: parseTree initialAnswer: true]
		ifFalse: [hasOnlySelfReturns]