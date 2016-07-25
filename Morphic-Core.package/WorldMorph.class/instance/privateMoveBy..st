private
privateMoveBy: delta
	super privateMoveBy: delta.
	worldState viewBox ifNotNil: [ worldState viewBox: bounds ]