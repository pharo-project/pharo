user interface
inputWidgetForColor
	^ (  
	 self theme 
		newColorChooserIn: World
		for: self
		getColor: #realValue
		setColor: #realValue:
		getEnabled: #enabled 
		help: 'Choose a color') minWidth: 150; width: 150; yourself