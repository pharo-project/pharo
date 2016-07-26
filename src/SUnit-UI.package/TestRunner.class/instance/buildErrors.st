building
buildErrors
	^ PluggableListMorph new
			on: self 
			list: #errorList
			selected: #errorSelected
			changeSelected: #errorSelected:
			menu: #errorMenu: 
			keystroke: nil.
	