shortcut-old
buildGroupShortcutsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #fullBrowse)
		category: #NautilusGroupShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the current selection'.
	(aBuilder shortcut: #restrictedBrowseGroup)
		category: #NautilusGroupShortcuts
		default: $b meta shift
		do: [ :target | target restrictedBrowseGroups: target packageWidget selectedGroups ]
		description: 'Open a browser on a restricted view'.
	(aBuilder shortcut: #addNewGroup)
		category: #NautilusGroupShortcuts
		default: $n meta
		do: [ :target | target addNewGroup ]
		description: 'Create a new group'.
	(aBuilder shortcut: #renameGroup)
		category: #NautilusGroupShortcuts
		default: $r meta
		do: [ :target | 
			target enableSingleMenuItems
				ifTrue: [ target renameGroup ] ]
		description: 'Rename the selected group'.
	(aBuilder shortcut: #runTestsOfGroups)
		category: #NautilusGroupShortcuts
		default: $t meta
		do: [ :target | target runTestsOfGroups: target packageWidget selectedGroups notifying: true ]
		description: 'Run the tests of the selected groups'.
	(aBuilder shortcut: #removeGroups)
		category: #NautilusGroupShortcuts
		default: $x meta
		do: [ :target | target removeGroups ]
		description: 'Remove the selected groups'