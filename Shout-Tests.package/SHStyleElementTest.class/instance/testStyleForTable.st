tests
testStyleForTable
	"
	self debug: #testStyleForTable
	"
	| table |
	table := element styleForTable.
	self assert: table size = 4.
	self assert: (table collect: #first) = element tokens