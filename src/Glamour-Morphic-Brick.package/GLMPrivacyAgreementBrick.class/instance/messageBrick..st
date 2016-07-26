accessing
messageBrick: anObject

	messageBrick := anObject.
	self messageBrick
		hSpaceFill;
		vAlign: #center;
		margin:10;
		text: self privacyMessage.

	self addBrickBack: self messageBrick.