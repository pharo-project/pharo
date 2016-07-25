private
getVertices: bounds
	"Construct vertices for a balloon up and to left of anchor"

	| corners |
	corners := bounds corners atAll: #(1 4 3 2).
	^ (Array
		with: corners first + (0 - bounds width // 2 @ 0)
		with: corners first + (0 - bounds width // 4 @ (bounds height // 2))) , corners