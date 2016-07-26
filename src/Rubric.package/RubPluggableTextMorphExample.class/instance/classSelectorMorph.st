view creation
classSelectorMorph
	^ self selectedClassName newTextField
		withGhostText: 'Class name';
		autoAccept: true;
		hResizing: #spaceFill;
		yourself