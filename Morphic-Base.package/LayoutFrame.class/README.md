I define a transformation frame relative to some rectangle. I'm basic data structure used for graphics.


Instance variables:
	leftFraction 
	topFraction 
	rightFraction 
	bottomFraction 	<Float>		The fractional distance (between 0 and 1) to place the morph in its owner's bounds
	leftOffset 
	topOffset 
	rightOffset 
	bottomOffset 	<Integer>	Fixed pixel offset to apply after fractional positioning (e.g., "10 pixel right of the center of the owner")