tests
testDefaultVersion
	
	self assert: (configuration selectors select: [ :s | s beginsWith: 'baseline']) size + 1 
		equals:	info baselines size
