building
buildButtonNoLabel
	^ PluggableButtonMorph
		on: self 
		getState: nil 
		action:  #action 
		label: nil 
		icon: #icon
		menu: nil. 	