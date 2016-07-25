private
applyStyle
	| table |
	table := Groups values flatCollect: [:group | group styleForTable].
	SHTextStylerST80 styleTable: table.
	SHRBTextStyler styleTable: table.