tests
testAtAtPut
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	d at: #top at: #below1 put: 2.
	d at: #fop at: #below1 put: 1.
	self assert: (d at: #top at: #below1) equals: 2.
	self assert: (d at: #fop at: #below1) equals: 1