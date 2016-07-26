menu
addTitleForHaloMenu: aMenu 
	aMenu buildTitle: [ :menuTitle | menuTitle
		bigTitle: self externalName;
		icon: (self iconOrThumbnailOfSize: 18);
		cellInset: 3
	]
