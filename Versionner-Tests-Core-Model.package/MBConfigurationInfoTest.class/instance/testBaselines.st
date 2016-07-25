tests
testBaselines

	"+1 is for the default: baseline"
	self assert: info baselines size = ((configuration selectors select: [ :s | s beginsWith: 'baseline']) size + 1).
