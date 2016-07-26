tests
testSearchingComposedSelector2

	searcher
		matches: '``@rcv at: ``@arg'
		do: [ :aNode :answer | answer + 1 ].
	"the result is 1 because the parse tree searcher only finds at: messages like at: and not ifAbsent: at:"	
	self 
		assert: (searcher 
					executeTree: (RBParser parseExpression: 'self ifAbsent: 2 at: 1; at: 4; foo')
					initialAnswer: 0)
		equals: 1.