menu
buildMenu: aMenu

	aMenu
		add: 'File in all selected'
		target: self
		selector: #fileIn:
		argumentList: { changes selectedItems }.
	
	aMenu
		addToggle: 'Compare method to current'
		target: self
		selector: #compareToCurrent:
		getStateSelector:nil
		enablementSelector: #canCompareToCurrent
		argumentList: { changes selectedItem }.

	^ aMenu