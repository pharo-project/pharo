I define a transformation frame relative to some rectangle. I'm basic data structure used for graphics.

Do not use fractions: fractionsOrNil offsets: offsetsOrNil or frations: fractionsOrNil if you do not have already  the rectangles that should be passed as arguments. 

If you are creating the rectangles representing the numbers you need, better use the accessors. For example like this.  

	(LayoutFrame identity
			topOffset: topHeight;
			bottomFraction: 0;
			bottomOffset: self buttonsBarHeight;
			leftOffset: -1;
			rightOffset: 1)	






Instance variables:
	leftFraction 
	topFraction 
	rightFraction 
	bottomFraction 	<Float>		The fractional distance (between 0 and 1) to place the morph in its owner's bounds
	leftOffset 
	topOffset 
	rightOffset 
	bottomOffset 	<Integer>	Fixed pixel offset to apply after fractional positioning (e.g., "10 pixel right of the center of the owner")