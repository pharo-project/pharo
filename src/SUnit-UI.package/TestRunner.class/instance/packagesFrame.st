building
packagesFrame
	^LayoutFrame new
		rightFraction: 0.25;
		topOffset: (Smalltalk ui theme textFont height + 13);
		bottomOffset: self buttonHeight negated