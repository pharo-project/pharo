tests - testing
testTabulate
	| m |
	m := self gridClass rows: 3 columns: 2 tabulate: [ :row :column | column * 10 + row ].
	self assert: (m atRow: 1 atColumn: 1) equals: 11.
	self assert: (m atRow: 1 atColumn: 2) equals: 21.
	self assert: (m atRow: 2 atColumn: 1) equals: 12.
	self assert: (m atRow: 2 atColumn: 2) equals: 22.
	self assert: (m atRow: 3 atColumn: 1) equals: 13.
	self assert: (m atRow: 3 atColumn: 2) equals: 23.