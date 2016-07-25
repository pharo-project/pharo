accessing
settingsButtonBrick: anObject
	settingsButtonBrick := anObject.
	
	self settingsButtonBrick
		text: self settingsButtonLabel;
		hShrinkWrap;
		vAlign: #center;
		margin:10;
		when: #onClicked do: [ self openSettingsBrowser ].
	
	self addBrickBack: self settingsButtonBrick