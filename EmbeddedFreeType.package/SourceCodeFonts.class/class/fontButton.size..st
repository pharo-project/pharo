accessing
fontButton: aString size: size 
	^ (Smalltalk ui theme 
		newButtonIn: World
			for: self
			getState: nil 
			action: #setSourceCodeFonts:	
			arguments: { size }
			getEnabled: nil
			getLabel: nil
			help: 'Change font style to Source Code Pro' translated)
		label: aString;
		yourself