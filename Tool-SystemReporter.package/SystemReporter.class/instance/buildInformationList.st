items creation
buildInformationList

	^ PluggableListMorph
		on: self
		list: #categoryList
		primarySelection: #categorySelected
		changePrimarySelection: #categorySelected:
		listSelection: #categoryAt:
		changeListSelection: #categoryAt:put:
		menu: nil
		keystroke: nil