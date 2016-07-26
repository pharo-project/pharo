private
buttonMorph: aModel
	^UITheme current 
		newButtonIn: nil 
		for: aModel 
		getState: nil
		action: #execute
		arguments: nil 
		getEnabled: nil 
		label: aModel buttonLabel
		help: nil