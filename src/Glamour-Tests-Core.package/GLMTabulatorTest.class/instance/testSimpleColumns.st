tests
testSimpleColumns
	| layout |
	layout := GLMTabulator new.
	layout addColumn: #first.
	layout addColumn: #second.
	self assert: layout cell children size equals: 2