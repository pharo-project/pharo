tests
testInexactNthRoot
	| tenth cubicRoot3 fifthRootTenth three |
	three := 3.0s1.
	cubicRoot3 := three raisedTo: 1/3.
	self assert: cubicRoot3 isFloat.
	self deny: cubicRoot3 squared = 3.
	tenth := 0.10s2.
	fifthRootTenth := tenth raisedTo: 1/5.
	self assert: fifthRootTenth isFloat.
	self deny: fifthRootTenth squared = tenth.