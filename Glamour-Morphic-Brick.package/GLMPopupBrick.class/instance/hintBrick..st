accessing
hintBrick: anObject
	hintBrick := anObject.
	
	
	self hintBrick
		textColor: self themer popupThemer textColor.
		
	self contentBrick addBrickBack: self hintBrick