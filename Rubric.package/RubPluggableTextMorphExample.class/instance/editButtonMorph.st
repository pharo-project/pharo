view creation
editButtonMorph
	^ UIManager default
		newButtonFor: self
		action: #editSourceCode
		getEnabled: nil
		label: 'Edit'
		help: nil