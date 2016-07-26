icon
groupIconFor: aGroup
	| icon |
	
	icon := IconicButton new 
				target: self;
				actionSelector: #restrictedBrowseGroups:;
				arguments: {{aGroup}};
				labelGraphic: (self iconNamed: #groupIcon);
				color: Color transparent;
				extent: 15 @ 16;
				helpText: 'Browse restricted environment';
				borderWidth: 0. 
	^ icon