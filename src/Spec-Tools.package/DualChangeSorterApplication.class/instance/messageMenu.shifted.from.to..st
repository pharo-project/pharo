menu
messageMenu: menu shifted: shifted from: sourcePanel to: destinationPanel
	"Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter"
	
	| menuModel menuDual |
	
	menuModel := MenuPresenter new.
	sourcePanel selectedSelector ifNil: [ ^ nil ].
	sourcePanel messageMenu: menu shifted: shifted.
	menuDual := MenuPresenter new fromSpec: (PragmaMenuBuilder 
		pragmaKeyword: 'dualChangeSorterMessageListMenu'
		model: { self. sourcePanel. destinationPanel }) 
		menuSpec.
	menuDual menuGroups do: [ :each | menuModel addMenuGroup: each ].
	menu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	^ menu