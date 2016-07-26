factory
buildWidget
	| label |
	label := LabelMorph new model: self.
	label
		getEnabledSelector: #enabled;
		getTextSelector: #getText;
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		setBalloonText: self help;
		color: self model color;
		emphasis: (self emphasisCodeFor: self model emphasis).
	^ label