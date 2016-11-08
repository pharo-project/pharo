initialize
addMenuButton
	| topOffset rightOffset |
	
	rightOffset := self actionOffset.
	topOffset := (self height - 16) // 2.
	self
		addMorph: menuButton
		fullFrame:
			(LayoutFrame identity
				bottomFraction: 0;
				leftFraction: 1;
				rightOffset: rightOffset negated;
				leftOffset: rightOffset negated - self menuButtonWidth;
				topOffset: topOffset;
				bottomOffset: topOffset + self menuButtonHeight)