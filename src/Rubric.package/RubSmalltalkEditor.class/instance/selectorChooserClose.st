basic-completion
selectorChooserClose
	selectorChooser 
		ifNotNil: [ [selectorChooser delete] ensure: [selectorChooser := nil] ]