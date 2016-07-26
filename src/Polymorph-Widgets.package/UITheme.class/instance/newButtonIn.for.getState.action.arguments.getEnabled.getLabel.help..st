morph creation
newButtonIn: aThemedMorph for: aModel getState: stateSel action: actionSel arguments: args getEnabled: enabledSel getLabel: labelSel help: helpText
	"Answer a new button."

	|b|
	b := PluggableButtonMorph
			on: aModel
			getState: stateSel
			action: actionSel
			label: labelSel.
	b
		theme: self;
		label: ' ' font: self buttonFont;
		update: labelSel;
		arguments: (args ifNil: [{b}]);
		getEnabledSelector: enabledSel;
		cornerStyle: (self buttonCornerStyleIn: aThemedMorph);
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: helpText;
		extent: b minExtent;
		removeProperty: #theme.
	^b