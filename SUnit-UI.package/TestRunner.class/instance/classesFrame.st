building
classesFrame
	^LayoutFrame new
		leftFraction: 0.25;
		topOffset: (Smalltalk ui theme textFont height + 13);
		rightFraction: 0.5;
		bottomOffset: self buttonHeight negated