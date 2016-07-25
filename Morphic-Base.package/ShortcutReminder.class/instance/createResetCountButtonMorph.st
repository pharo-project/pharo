settings
createResetCountButtonMorph

	^SimpleButtonMorph new
		target: self;
		label: 'Reset notification counts';
		actionSelector: #resetCount;
		themeChanged;
		yourself
	