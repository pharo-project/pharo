building
buildFailures
	^ PluggableListMorph new
			on: self 
			list: #failedList
			selected: #failedSelected
			changeSelected: #failedSelected:
			menu: #failureMenu: 
			keystroke: nil.
	
		" do not know how to express
		name: 'Failure List'"