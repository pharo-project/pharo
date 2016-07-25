actions
newContentMorph
	| panel  |
	panel := PanelMorph new.
	panel layoutPolicy: ProportionalLayout new;
		layoutInset: 0;
		hResizing: #spaceFill;
		vResizing: #spaceFill.					
	panel addMorph: self buildListMorph
			fullFrame: (LayoutFrame identity bottomOffset: -33).
	panel addMorph: self buildSearchMorph
			fullFrame: ((0@1 corner: 1@1) asLayoutFrame topOffset: -30).
	^ panel