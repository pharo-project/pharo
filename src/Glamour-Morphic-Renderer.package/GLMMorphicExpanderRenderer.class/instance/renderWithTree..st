rendering
renderWithTree: aBrowser
	| treeMorph treeModel |
	treeModel := GLMExpanderTreeMorphModel new glamourExpander: aBrowser.
	treeMorph := MorphTreeMorph new.
	treeMorph
		makeLastColumnUnbounded;
		getMenuSelector: #menu:shifted:;
		keystrokeActionSelector: #keyStroke:from:;
		cornerStyle: treeMorph preferredCornerStyle;
		borderStyle: (BorderStyle inset width: 1);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		layoutFrame: (LayoutFrame identity);
		preferedPaneColor: Color white;	
		model: treeModel;
		columns: ( OrderedCollection with: (MorphTreeColumn new rowMorphGetSelector: #elementColumn)).
	treeMorph vShowScrollBar.
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self.
	^ treeMorph buildContents