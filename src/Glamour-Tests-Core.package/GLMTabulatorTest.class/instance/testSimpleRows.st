tests
testSimpleRows
	| layout |
	layout := GLMTabulator new.
	layout addRow: #first.
	layout addRow: #second.
	self assert: layout cell children size equals: 2.
	self assert: layout cell isColumn.
	self assert: layout cell children first isRow