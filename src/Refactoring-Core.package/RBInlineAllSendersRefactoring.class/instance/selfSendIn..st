transforming
selfSendIn: aTree 
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: self messagePattern do: [:aNode :answer | ^aNode].
	^searcher executeTree: aTree initialAnswer: nil