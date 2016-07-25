user-interface
newFindButton
	^(self theme
			newButtonIn: self
			for: self
			getState: nil
			action: #find
			arguments: nil
			getEnabled: #findEnabled
			label: 'Find' translated
			help: 'Find the next occurence' translated)
		hResizing: #rigid;
		vResizing: #rigid