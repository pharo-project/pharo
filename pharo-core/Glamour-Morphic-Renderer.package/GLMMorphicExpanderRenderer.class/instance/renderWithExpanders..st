rendering
renderWithExpanders: aBrowser
	| morph scroll panel expander |
	panel := PanelMorph new.
	panel 
		fillStyle: Color transparent;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		changeTableLayout;
		layoutInset: 2;
		cellInset: 2.
	scroll := GeneralScrollPane new.
	scroll 
		changeScrollerTableLayout;
		scrollTarget: panel.
	aBrowser panes do: [:each |
		expander := ExpanderMorph titleText: (each presentations titleValue ifNil: ['noname']).
		morph := GLMMorphicPaneWithoutTitleRenderer new render: each.
		morph 
			borderStyle: (UITheme current buttonSelectedBorderStyleFor: morph).
		expander addMorphBack: morph.
		panel addMorph: expander	].
	^ scroll