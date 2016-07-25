newbuilding
buildStatus

	^ (PluggableTextFieldMorph
		on: self 
		text: #statusText 
		accept: nil 
		readSelection: nil 
		menu: #statusMenu:)
			textColor: self theme textColorForNonStandardBackground;
			getColorSelector: #statusColor ; 
			yourself.
	