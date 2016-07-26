accessing
contentBrick: aBrick

	contentBrick := aBrick.
	self contentBrick
		vShrinkWrap;
		hShrinkWrap;
		padding: self themer popupThemer borderWidth;
		color: self themer popupThemer backgroundColor; 
		borderColor: self themer popupThemer borderColor;
	 	borderWidth: self themer popupThemer borderWidth.
	self addBrickBack: contentBrick

	