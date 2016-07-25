shortcuts
buildAddShortcutsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #newClass)
		category: #NautilusGlobalShortcuts
		default: $n meta , $c meta
		do: [ :target | target addClass ]
		description: 'Create a new class'.
	(aBuilder shortcut: #newPackage)
		category: #NautilusGlobalShortcuts
		default: $n meta , $p meta
		do: [ :target | target addPackage ]
		description: 'Create a new package'.
	(aBuilder shortcut: #newProtocol)
		category: #NautilusGlobalShortcuts
		default: $n meta , $t meta
		do: [ :target | target addCategory ]
		description: 'Create a new protocol'.
	(aBuilder shortcut: #newGroup)
		category: #NautilusGlobalShortcuts
		default: $n meta , $g meta
		do: [ :target | target addNewGroup ]
		description: 'Create a new group'