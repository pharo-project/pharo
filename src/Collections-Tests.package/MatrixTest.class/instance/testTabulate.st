tests - testing
testTabulate
	| m |
	m := Matrix rows: 3 columns: 2 tabulate: [ :row :column | column * 10 + row ].
	self assert: (m at: 1 at: 1) equals: 11.
	self assert: (m at: 1 at: 2) equals: 21.
	self assert: (m at: 2 at: 1) equals: 12.
	self assert: (m at: 2 at: 2) equals: 22.
	self assert: (m at: 3 at: 1) equals: 13.
	self assert: (m at: 3 at: 2) equals: 23.