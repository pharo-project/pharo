as yet unclassified
layOutGeometry

	"first do a vertical layout"
	
	| origin |
	
	origin := 0.
	
	
	rows do: [:row |  | rowHeight color |
		
		rowHeight := 0.
		color := Color random.
		
		row do: [ :node |
			node originY: origin.
			node color: color.
			rowHeight := rowHeight max: node height.
		].
	
		origin := origin + rowHeight + self spacingBetweenRows.
	].

	"now do a horizontal one"
	
	root layoutHorizontally: 0.