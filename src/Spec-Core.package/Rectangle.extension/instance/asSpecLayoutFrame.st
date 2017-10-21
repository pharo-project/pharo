*Spec-Core
asSpecLayoutFrame
	"Answer an instance of LayoutFrame which uses fractions from receiver "
	
	^ SpecLayoutFrame new
		leftFraction: self left;
		rightFraction: self right;
		topFraction: self top;
		bottomFraction: self bottom;
		yourself
		