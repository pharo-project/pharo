costly instance creation
fractions: fractionsOrNil
	"Do not use this API if you do not have already have the rectangles that should be passed as argument. 
	If you are creating the rectangles representing the numbers you need, better use the accessors: 
	(LayoutFrame identity
			topOffset: topHeight;
			bottomFraction: 0;
			bottomOffset: self buttonsBarHeight;
			leftOffset: -1;
			rightOffset: 1)	
	
	In particular 
	do not use (LayoutFrame fractions: (0 @ 0 corner: 1 @ 1)) but favor 
	LayoutFrame identity which is faster and more readable.
	"
	^ self fractions: fractionsOrNil offsets: nil