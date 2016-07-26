menu
changesMenu: menu shifted: shifted from: sourcePanel to: destinationPanel
	| menuModel menuDest |

	menuModel := MenuModel new.
	menu addTitle: 'Change Set'.
	sourcePanel changeSetMenu1: menu shifted: shifted.
	menuDest := MenuModel new
		fromSpec:
			(PragmaMenuBuilder
				pragmaKeyword: 'dualChangeSorteChangesListMenu'
				model:
					{self.
					sourcePanel.
					destinationPanel}) menuSpec.
	menuDest menuGroups do: [ :each | menuModel addMenuGroup: each ].
	menu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	sourcePanel changeSetMenu2: menu shifted: shifted.
	^ menu