*Morphic-Base
asLayoutFrame
	"Answer an instance of LayoutFrame which uses fractions from receiver "
	
	^ LayoutFrame new
		leftFraction: self left;
		rightFraction: self right;
		topFraction: self top;
		bottomFraction: self bottom;
		yourself
		