user-interface
newReplaceButton
	^ (self theme
		newButtonIn: self
		for: self
		getState: nil
		action: #replace 
		arguments: nil
		getEnabled: #replaceEnabled
		label: 'Replace' translated
		help: 'Replace the next occurence' translated) hResizing: #rigid;
		 vResizing: #rigid