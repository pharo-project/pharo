accessing
closeButton: aBrick

	closeButton ifNotNil: [self removeBrick: closeButton].
	closeButton := aBrick.
	
	self themer spotterThemer closeButtonWidgetStyleFor: closeButton.
	self themer spotterThemer closeButtonStyleFor: closeButton.
	
	closeButton
		hintTitle: 'Close Spotter';
		hintText: '<escape>';
		action: [ self spotterModel exitDueTo: #actionButton ].
	
	self addBrickBack: closeButton