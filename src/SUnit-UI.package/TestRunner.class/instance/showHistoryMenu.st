history saving
showHistoryMenu
	| selectionIndex selectedPreviousResult actionIndex |
	selectionIndex := UIManager default chooseFrom: self historyMenuList title: 'History:'.

	"We pressed outside the menu"
	selectionIndex isZero ifTrue: [ ^ self ]. 				

	"save current result is selected"
	selectionIndex = 1 ifTrue: [ self previousRun addFirst: result. ^ self ]. 			

	selectedPreviousResult := self previousRun at: (selectionIndex - 1).
 	actionIndex := (UIManager default chooseFrom: #('delete' 'show diff')  title:  'Action:').
	actionIndex = 1 ifTrue: [ self previousRun remove: selectedPreviousResult. ^ self ].
	actionIndex = 2 ifTrue: [ self showDiffWith: selectedPreviousResult].	