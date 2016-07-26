recategorized
privateMoveBy: delta
	"Adjust all the fill styles"
	
	super privateMoveBy: delta.
	(self fillStyles copyWithout: self fillStyle) do: [:fs | fs isOrientedFill ifTrue: [fs origin: fs origin + delta]]