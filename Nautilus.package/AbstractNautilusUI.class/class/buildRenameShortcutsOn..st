shortcuts
buildRenameShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #renameClass)
		category: #NautilusGlobalShortcuts
		default: $r meta  ,$c meta 
		do: [ :target | target renameClass ]
		description: 'Rename the selected class'.
			
	(aBuilder shortcut: #renamePackage)
		category: #NautilusGlobalShortcuts
		default: $r meta  , $p meta 
		do: [ :target | target renamePackage ]
		description: 'Rename the selected package'.

	(aBuilder shortcut: #renameProtocol)
		category: #NautilusGlobalShortcuts
		default: $r meta , $t meta 
		do: [ :target | target renameCategory ]
		description: 'Rename the selected protocol'.
		
	(aBuilder shortcut: #renameGroup)
		category: #NautilusGlobalShortcuts
		default: $r meta , $g meta 
		do: [ :target | target renameGroup ]
		description: 'Rename the selected group'.