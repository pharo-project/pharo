tabs creation
redMorphTab
	| tab |
	tab := TabPresenter new.
	tab
		label: 'Red Morph';
		icon: (tab iconNamed: #smallBackIcon);
		contents:
			(Morph new
				color: Color red;
				yourself).
	^ tab