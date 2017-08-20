tests
testAtAtIfAbsentPut
	| d |
	d := Dictionary new.
	d at: #first at: #second put: [ 2 ].
	d at: #first at: #second42 ifAbsentPut: [ 142 ].
	d at: #first42 at: #second ifAbsentPut: [ 42 ].
	d at: #first42 at: #second42 ifAbsentPut: [ 0 ].
	self assert: (d at: #first at: #second42) equals: 142.
	self assert: (d at: #first42 at: #second) equals: 42.
	self assert: (d at: #first42 at: #second42) equals: 0.
	