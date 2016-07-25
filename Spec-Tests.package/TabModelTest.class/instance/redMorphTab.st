instance creation
redMorphTab
	^ TabModel new
		label: 'Red Morph';
		icon: (Smalltalk ui icons iconNamed: #smallBackIcon);
		contents:
				(Morph new
						color: Color red;
						yourself);
		yourself