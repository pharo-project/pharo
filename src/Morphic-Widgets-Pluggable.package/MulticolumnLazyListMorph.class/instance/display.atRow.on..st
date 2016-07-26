row management
display: items atRow: row on: canvas
	"display the specified item, which is on the specified row; for Multicolumn 
	lists, items will be a list of strings"

	| drawBounds backgroundColor |
	backgroundColor := self backgroundColorForRow: row.
	drawBounds := self drawBoundsForRow: row.
	drawBounds := drawBounds
		intersect: self bounds
		ifNone: [ 
			"oh well"
			^ self ].
	items
		with: (1 to: items size)
		do: [ :item :index | 
			"move the bounds to the right at each step"
			index > 1
				ifTrue: [ drawBounds := drawBounds left: drawBounds left + listSource gapSize + (columnWidths at: index - 1) ].
			item
				listRenderOn: canvas
				atRow: row
				bounds: drawBounds
				color: color
				backgroundColor: backgroundColor
				from: self ]