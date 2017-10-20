tests
testAtAtputEffectivelyPutAValue
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	self assert: (d at: #top at: #below1) equals: 1