tests
testSearchingComposedSelector
	
	searcher 
		matches: '``@rcv at: ``@arg `sel: ``@arg1'
		do: [ :aNode :answer | answer + 1].
		
	"the result is 2 because the parse tree searcher only finds at:x: messages like at:put: and at:ifAbsent:"	
	self 
		assert: (searcher 
					executeTree: (RBParser 	
							parseExpression: 'self at: 1 put: 2; at: 2 ifAbsent: []; ifAbsent: 2 at: 1; at: 4; foo')
					initialAnswer: 0) = 2.
	