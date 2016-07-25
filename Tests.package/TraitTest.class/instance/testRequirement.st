testing
testRequirement
	"self debug: #testRequirement"

	self t1 compile: 'm self requirement'.
	self t2 compile: 'm ^true'.
	self assert: self c2 new m.
	self t2 removeSelector: #m.
	self should: [self c2 new m] raise: Error