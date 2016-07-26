transforming
translateBy: delta 
	"Answer a Point translated by delta (an instance of Point)."

	^ (delta x + x) @ (delta y + y)