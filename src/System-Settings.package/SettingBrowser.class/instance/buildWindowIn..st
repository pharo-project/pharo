user interface
buildWindowIn: aWindow 
	| statusView toolBar treeMorph toolBarY gap packageListView statusViewHeight |
	treeMorph := self treeMorphIn: aWindow.
	statusView := self statusViewIn: aWindow.
	statusViewHeight := 100.
	toolBar := self toolBarIn: aWindow.
	gap := 3.
	packageListView := self packageListViewIn: aWindow.
	toolBarY := toolBar minExtent y + gap.
	aWindow 
		addMorph: toolBar
		fullFrame: ((0 @ 0 corner: 1 @ 0) asLayoutFrame 
						topOffset: gap;
						bottomOffset: toolBarY + gap).
	aWindow 
		addMorph: treeMorph
		fullFrame: ((0 @ 0 corner: 1 @ 0.99) asLayoutFrame 
				topOffset: toolBarY + gap + gap;
				bottomOffset: statusViewHeight negated).
	aWindow 
		addMorph: statusView
		fullFrame: ((0 @ 0.99 corner: 0.75 @ 1) asLayoutFrame 
				topOffset: statusViewHeight negated).
	aWindow 
		addMorph: packageListView
		fullFrame: ((0.75 @ 0.99 corner: 1 @ 1) asLayoutFrame 
				topOffset: statusViewHeight negated).
	^ aWindow