settings
createIconPackList
	^ (Smalltalk ui theme 
		newDropListIn: World
		for: self
		list: #availablePacks
		getSelected: #current
		setSelected: #current:
		getEnabled: nil
		useIndex: false
		help: nil)
		wrapSelector: #name;
		hResizing: #rigid;
		width: 120;
		yourself