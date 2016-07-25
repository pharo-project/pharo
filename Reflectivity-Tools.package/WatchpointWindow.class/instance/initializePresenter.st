initialization
initializePresenter 
	
	list whenSelectionChanged: [inspectIt enabled: (list getIndex = 0) not ].
	
	"have to do it this way because list selectedItem is the originally selected item"
	inspectIt action: [ |index |
		index := list getIndex.
		index = 0 ifFalse: [ (list listItems at: index) inspect ]]