item creation
packageTreeBox
	^ GroupboxMorph new
		addContentMorph: self buildPackageTree;
		label: 'Select an item:';
		hResizing: #spaceFill;
		vResizing: #spaceFill