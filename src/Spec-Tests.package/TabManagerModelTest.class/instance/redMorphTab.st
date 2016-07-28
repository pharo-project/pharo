tabs creation
redMorphTab
	| tab |
	tab := TabModel new.
	tab
		label: 'Red Morph';
		icon: (tab iconNamed: #smallBackIcon);
		contents:
			(Morph new
				color: Color red;
				yourself).
	^ tab