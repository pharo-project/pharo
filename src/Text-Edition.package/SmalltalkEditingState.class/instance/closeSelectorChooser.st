selector-chooser
closeSelectorChooser
	selectorChooser 
		ifNotNil: [ [selectorChooser delete] ensure: [selectorChooser := nil] ]