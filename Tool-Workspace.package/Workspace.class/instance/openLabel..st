view
openLabel: aString
	
	window := StandardWindow new model: self.
	window title: aString.
	window
		addMorph: self textView
		fullFrame: LayoutFrame identity.
	window openInWorld
	

