accessing
triangleBrick: aBrick

	triangleBrick := aBrick.
	self triangleBrick
		hintTitle: 'Toggle preview';
		hintText: '<',OSPlatform current defaultModifier name asLowercase,'+p>';
		action: [ self spotterModel togglePreview ].
	self themer spotterThemer spotterPreviewArrowStyleFor: self triangleBrick.
	self addBrickBack: self triangleBrick
	