display
buildTaskList
	^ (PluggableListMorph
		on: self
		list: #tasks
		selected: #selectedIndex
		changeSelected: #selectedIndex:
		menu: nil
		keystroke: nil)
		height: 50;
		hResizing: #spaceFill;
		yourself