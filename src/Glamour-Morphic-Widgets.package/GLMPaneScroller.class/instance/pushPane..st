panes
pushPane: aMorph	
	aMorph 
		borderWidth: 0;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		layoutInset: 0.
	transform hasSubmorphs 
		ifTrue: [ transform addMorphBack: self separator ].
	transform addMorphBack: (aMorph adoptPaneColor: self paneColor).
	paneCount := paneCount + 1.
	self updatePanes