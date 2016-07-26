user interface
getFont
	^ FontChooser
		openWithWindowTitle: 'Font for ' , self name
		for: self
		setSelector: #realValue:
		getSelector: #realValue 