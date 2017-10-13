menu
classMenu: menu shifted: shifted from: sourcePanel to: destinationPanel
	"Fill aMenu with items appropriate for the class list"
	| menuModel menuDest |
	
	menuModel := MenuPresenter new.
	sourcePanel selectedClass ifNil: [ ^ nil ].
	sourcePanel classMenu: menu shifted: shifted.
	menuDest := MenuPresenter new fromSpec: (PragmaMenuBuilder
		pragmaKeyword: 'dualChangeSorterClassListMenu'
		model: {self. sourcePanel. destinationPanel}) 
		menuSpec.
	menuDest menuGroups do: [ :each | menuModel addMenuGroup: each ].
	menu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	^ menu