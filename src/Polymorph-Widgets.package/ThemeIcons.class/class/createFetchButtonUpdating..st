settings
createFetchButtonUpdating: listMorph
	^ (Smalltalk ui theme 
		newButtonIn: World
		for: self
		getState: nil 
		action: nil
		arguments: nil
		getEnabled: nil
		getLabel: nil
		help: 'Fetch icon pack from remote repository' translated)
		label: 'Fetch from remote';
		actionBlock: [ 
			self uiFetchPacks.
			listMorph update: #availablePacks ];
		yourself