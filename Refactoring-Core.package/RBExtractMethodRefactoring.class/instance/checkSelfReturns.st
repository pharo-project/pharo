transforming
checkSelfReturns
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher
		matches: '^self' do: [:aNode :answer | answer];
		matches: '^`@anything' do: [:aNode :answer | true].
	(searcher executeTree: extractedParseTree initialAnswer: false) 
		ifTrue: [self placeholderNode asReturn]