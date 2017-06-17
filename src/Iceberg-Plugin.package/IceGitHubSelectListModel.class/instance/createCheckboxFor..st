private factory
createCheckboxFor: anObject
	^ CheckboxMorph 
		on: (IceGitHubSelectItem list: selectedItems item: anObject) 
		selected: #value 
		changeSelected: #value: