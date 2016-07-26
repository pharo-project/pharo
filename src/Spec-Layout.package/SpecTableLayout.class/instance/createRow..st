private
createRow: aBlock
	| row |
	row :=  SpecTableLayout row.
	aBlock value: row.
	^ row