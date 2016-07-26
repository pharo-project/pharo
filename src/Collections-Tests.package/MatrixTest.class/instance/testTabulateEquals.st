tests - testing
testTabulateEquals
	| m |
	m := Matrix rows: 3 columns: 2 tabulate: [ :row :column | column * 10 + row ].
	self assert: (m = matrix23)
