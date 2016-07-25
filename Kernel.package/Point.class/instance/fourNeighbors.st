point functions
fourNeighbors
	^ Array with: self + (1 @ 0)
		with: self + (0 @ 1)
		with: self + (-1 @ 0)
		with: self + (0 @ -1)
