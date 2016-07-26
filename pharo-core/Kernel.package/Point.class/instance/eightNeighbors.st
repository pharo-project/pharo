point functions
eightNeighbors
	^ { self + (1 @ 0) . self + (1 @ 1) .  self + (0 @ 1) . self + (-1 @ 1) . self + (-1 @ 0) .
		 self + (-1 @ -1) . self + (0 @ -1) . self + (1 @ -1)}
