morph
selectedMorph
	^ (TickSelectionCheckboxMorph basicNew index: (self model rootItems indexOf: self item);
		initialize;	
		model: self item;
		getStateSelector: #selected;
		setStateSelector: #selected:;
		updateSelection;
		getEnabledSelector: nil;
		font: StandardFonts defaultFont;
		label: '';
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: nil) color: Color transparent; yourself