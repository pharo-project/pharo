adding
addDiveInButton
	
	diveInButton := self newActionbarButton.
	self diveInButton
		icon: self themer spotterThemer iconDiveInCategory;
		pressedColor: (self themer spotterThemer itemActionSelectedBackgroundColor alpha: 0.5);
		action: [ self stepModel diveInCategory: self category dueTo: #actionButton ];
		beHelp;
		hintTitle: 'Show all';
		hintText: '<',OSPlatform current defaultModifier name asLowercase,'+shift+right>'.
		
	self pushActionButton: self diveInButton
	