accessing
titleBrick: aBrick
	titleBrick := aBrick.
	
	self titleBrick
		textColor: self themer popupThemer textColor.
		
	self contentBrick addBrickFirst: self titleBrick