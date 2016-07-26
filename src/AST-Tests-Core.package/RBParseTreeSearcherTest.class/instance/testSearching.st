tests - to de refined
testSearching

	searcher matches: '``@rcv at: ``@arg `sel: ``@arg1'
		do: [:aNode :answer | answer + 1].
	self 
		assert: (searcher executeTree: (RBParser 
						parseExpression: 'self at: 1 put: 2; at: 2 ifAbsent: []; ifAbsent: 2 at: 1; at: 4; foo')
				initialAnswer: 0) = 2.
	searcher := RBParseTreeSearcher new.
	searcher matches: '``@rcv `at: ``@arg1 `at: ``@arg2'
		do: [:aNode :answer | answer + 1].
	self 
		assert: (searcher executeTree: (RBParser 
						parseExpression: 'self at: 1 at: 3; at: 1 put: 32; at: 2; foo; at: 1 at: 1 at: 2')
				initialAnswer: 0) = 1.
	searcher := RBParseTreeSearcher new.
	searcher matchesMethod: 'at: `object `put: `o1 ``@rcv `put: 1'
		do: [:aNode :answer | true].
	self assert: (searcher 
				executeTree: (RBParser parseMethod: 'at: a put: b self foo put: 1')
				initialAnswer: false)