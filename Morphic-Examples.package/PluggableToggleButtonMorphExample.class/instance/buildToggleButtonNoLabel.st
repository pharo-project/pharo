building
buildToggleButtonNoLabel
	^ PluggableToggleButtonMorph
		on: self 
		getState: nil
		action:  #toggleAction:
		label: nil 
		icon: #icon
		menu: nil. 	