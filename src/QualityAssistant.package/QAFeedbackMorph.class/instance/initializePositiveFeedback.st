initialization
initializePositiveFeedback
	
	^ positive := IconicButton new
				target: self;
				labelGraphic: (Smalltalk ui icons iconNamed: #thumbsUpIcon);
				actionSelector: #fireDialogWithFeedback:;
				arguments: { true };
				color: Color transparent;
				helpText: 'This critic is helpful';
				extent: 16 @ 16;
				borderWidth: 0;
				yourself