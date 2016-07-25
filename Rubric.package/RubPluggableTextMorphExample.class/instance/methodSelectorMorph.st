view creation
methodSelectorMorph
	^ self selectedMethodName newTextField
		withGhostText: 'Method name';
		autoAccept: true;
		hResizing: #spaceFill;
		yourself