private-icons
addIcon

	self
		addMorph: closeIcon 
		fullFrame: (LayoutFrame identity
			bottomFraction: 0;
			leftFraction: 1;
			topOffset: 8; "tab height : 26px, icon height: 12 px -> (26 - 12 / 4) + 1"
			leftOffset: self iconRightOffset negated - 12;
			rightOffset: self iconRightOffset negated;
			bottomOffset: 20; "8 + 12"
			yourself)