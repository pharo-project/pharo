private
addLabel

	self
		addMorph: label 
		fullFrame: (LayoutFrame identity
			topFraction: 1;
			leftOffset: self labelRightOffset;
			topOffset: (label height negated - self labelBottomOffset);
			yourself)
			