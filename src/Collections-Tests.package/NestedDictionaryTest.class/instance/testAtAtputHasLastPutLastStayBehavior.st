tests
testAtAtputHasLastPutLastStayBehavior
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	d at: #top at: #below1 put: 2.
	self assert: (d at: #top at: #below1) equals: 2