tests
testOneSimpleRowAndOneNested
	| layout |
	layout := GLMTabulator new.
	layout addRow: #first.
	layout
		addRow: [ :row | 
			row addColumn: #second.
			row addColumn: #third.
			row addColumn: #fourth ].
	self assert: layout cell children size equals: 2.
	self assert: layout cell children last children size equals: 3