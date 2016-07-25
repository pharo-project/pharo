accessing
on: anObject getState: getStateSel action: actionSel label: labelSel icon: iconSel menu: menuSel
	"Set up the pluggable parameters.
	Update label and state."

	super
		on: anObject
		getState: getStateSel
		action: actionSel
		label: labelSel
		icon: iconSel
		menu: menuSel.
	self pressed: super getModelState