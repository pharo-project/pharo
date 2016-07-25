accessing
titleBrick: anObject
	titleBrick := anObject.
	
	self titleBrick
		textColor: self themer popupThemer textColor.
		
	self contentBrick addBrickFirst: self titleBrick