factory
buildWidget
	^ CheckboxMorph new
		on: self selected: #state changeSelected: #state:;
		getEnabledSelector: #enabled;
		label: self label;
		labelClickable: self labelClickable;
		beRadioButton;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		yourself