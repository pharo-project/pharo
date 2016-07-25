morph creation
newButtonIn: aThemedMorph for: aModel getState: stateSel action: actionSel arguments: args getEnabled: enabledSel label: label help: helpText
	"Answer a new button."

	|b|
	b := PluggableButtonMorph
			on: aModel getState: stateSel action: actionSel.
	b
		theme: self;
		arguments: (args ifNil: [{b}]);
		cornerStyle: (self buttonCornerStyleIn: aThemedMorph);
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		label: label font: self buttonFont;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText;
		extent: b minExtent;
		hResizing: #rigid;
		vResizing: #rigid;
		removeProperty: #theme.
	^b