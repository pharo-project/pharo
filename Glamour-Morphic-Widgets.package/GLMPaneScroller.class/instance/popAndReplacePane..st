panes
popAndReplacePane: aMorph	
	transform removeMorph: transform lastSubmorph. "one for the separator"
	transform submorphs isEmpty
		ifFalse: [ transform removeMorph: transform lastSubmorph ].
	paneCount := paneCount - 1.	
	
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