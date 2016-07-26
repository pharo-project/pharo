tests
testComplex
	| layout |
	layout := GLMTabulator new.
	layout
		addColumn: [ :column | 
			column addRow: #second.
			column addRow: #third.
			column addRow: #fourth.
			column
				addRow: [ :row | 
					row addColumn: #fifth.
					row addColumn: #sixth ] ].
	layout addColumn: #first.
	self assert: layout cell children size equals: 2.
	self assert: layout cell children first children size equals: 4.
	self assert: layout cell children first children last children size equals: 2