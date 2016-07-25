private
createColumn: aBlock
	| column |
	column := SpecTableLayout column.
	aBlock value: column.
	^ column