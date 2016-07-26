view creation
metaSwitchMorph
	^ UIManager default
		newCheckboxFor: self
		getSelected: #classIsMeta
		setSelected: #classIsMeta:
		label: 'Class'
		help: 'Class side is used if checked' 