tabs creation
redMorphTab
	^ TabModel new
		label: 'Red Morph';
		icon: Smalltalk ui icons smallBackIcon;
		contents:
			(Morph new
				color: Color red;
				yourself);
		yourself