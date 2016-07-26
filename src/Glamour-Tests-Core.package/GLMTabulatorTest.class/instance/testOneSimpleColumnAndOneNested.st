tests
testOneSimpleColumnAndOneNested
	| layout |
	layout := GLMTabulator new.
	layout
		addColumn: [ :column | 
			column addRow: #second.
			column addRow: #third.
			column addRow: #fourth ].
	layout addColumn: #first.
	self assert: layout cell children size equals: 2.
	self assert: layout cell children first children size equals: 3