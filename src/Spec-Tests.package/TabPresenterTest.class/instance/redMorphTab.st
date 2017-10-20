instance creation
redMorphTab
	testedInstance
		label: 'Red Morph';
		icon: (testedInstance iconNamed: #smallBackIcon);
		contents:
			(Morph new
				color: Color red;
				yourself).