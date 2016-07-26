interface building
newLanguageList

	| widget |
	widget := PluggableListMorph
		on: self 
		list: #languages
		selected: #languageIndex
		changeSelected: #languageIndex:.
	^ widget	
			color: Color white;
			borderInset;
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself.