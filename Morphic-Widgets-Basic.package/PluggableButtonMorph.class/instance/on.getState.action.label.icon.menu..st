accessing
on: anObject getState: getStateSel action: actionSel label: labelSel icon: iconSel menu: menuSel
	"Set up the pluggable parameters.
	Update label and state."
	
	self model: anObject.
	getStateSelector := getStateSel.
	actionSelector := actionSel.
	getLabelSelector := labelSel.
	getIconSelector := iconSel.
	getMenuSelector := menuSel.
	self
		update: (labelSel ifNil: [ iconSel ]);
		update: getStateSel
