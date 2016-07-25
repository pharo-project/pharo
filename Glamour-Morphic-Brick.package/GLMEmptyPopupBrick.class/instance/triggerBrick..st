accessing
triggerBrick: aBrick

	triggerBrick := aBrick.
	self triggerBrick
		color: self themer popupThemer backgroundColor;
		borderColor: self themer popupThemer borderColor;
		borderWidth: self themer popupThemer borderWidth;
		action: [ :b :evt | self close. self anchorBrick click: evt ].
		
	self addBrickBack: self triggerBrick.