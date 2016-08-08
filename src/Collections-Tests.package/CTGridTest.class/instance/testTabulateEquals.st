tests - testing
testTabulateEquals
	| m |
	m := CTGrid rows: 3 columns: 2 tabulate: [ :row :column | column * 10 + row ].
	self assert: (m = grid23)
