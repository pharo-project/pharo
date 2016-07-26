user interface
inputWidgetForFont
	^ (  self theme 
		newButtonIn: World
		for: self
		getState: nil 
		action: #getFont
		arguments: {}
		getEnabled: #enabled
		getLabel: #fontButtonLabel
		help: 'Choose a font ...' translated) cornerStyle: #square
