user interface
inputWidgetForFileOrDirectoryWithAction: aSymbol
	| list button |
	list := EditableDropListMorph
		on: self
		list: #list
		selected: #index
		changeSelected: #index:
		useIndex: true
		addToList: #addToList:
		class: String
		getEnabled: #enabled
		default: self defaultValue.
	list
		ghostText: self ghostHelp;
		hResizing: #rigid;
		width: 450;
		wantsFrameAdornments: true.
	button := self theme
		newButtonIn: World
		for: self
		getState: nil
		action: aSymbol
		arguments: nil
		getEnabled: nil
		label:
			(AlphaImageMorph new
				image: (Smalltalk ui icons iconNamed: #smallOpenIcon))
		help: 'Open directory chooser dialog' translated.
	^ self theme
		newRowIn: World
		for:
			{list.
			button}