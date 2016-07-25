shortcuts
buildBrowseShortcutsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #browseSenders)
		category: #NautilusGlobalShortcuts
		default: $b meta , $n meta
		do: [ :target | target browseSendersOfMessages ]
		description: 'Browse senders of the selected method'.
	(aBuilder shortcut: #browseClassRefs)
		category: #NautilusGlobalShortcuts
		default: $b meta , $n meta shift
		do: [ :target | target browseClassRefs ]
		description: 'Browse class references'.
	(aBuilder shortcut: #browseImplementors)
		category: #NautilusGlobalShortcuts
		default: $b meta , $m meta
		do: [ :target | target browseMessages ]
		description: 'Browse implementors of the selected method'.
	(aBuilder shortcut: #browseRestricted)
		category: #NautilusGlobalShortcuts
		default: $b meta , $s meta
		do: [ :target | target restrictedBrowse ]
		description: 'Open a restricted browser'.
	(aBuilder shortcut: #browseRestrictedClass)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $c meta
		do: [ :target | target restrictedBrowseClass ]
		description: 'Open a restricted browser on the selected class'.
	(aBuilder shortcut: #browseSuperclass)
		category: #NautilusGlobalShortcuts
		default: $b meta , $s meta shift
		do: [ :target | target browseSuperclass ]
		description: 'Open a browser on the superclass of the selected class'.
	(aBuilder shortcut: #browseRestrictedPackage)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $p meta
		do: [ :target | target restrictedBrowsePackage ]
		description: 'Open a restricted browser on the selected package'.
	(aBuilder shortcut: #browseRestrictedRegex)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $r meta
		do: [ :target | target restrictedBrowsePackageRegex ]
		description: 'Open a restricted browser on a package regex'.
	(aBuilder shortcut: #browseRestrictedSuperclasses)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $S meta shift
		do: [ :target | target restrictedBrowseSuperclasses ]
		description: 'Open a restricted browser the superclasses'.
	(aBuilder shortcut: #browseRestrictedSubclasses)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $s meta
		do: [ :target | target restrictedBrowseSubclasses ]
		description: 'Open a restricted browser the subclasses'.
	(aBuilder shortcut: #browseInheritance)
		category: #NautilusGlobalShortcuts
		default: $b meta , $i meta
		do: [ :target | target methodHierarchy ]
		description: 'Open a restricted browser'.
	(aBuilder shortcut: #browseFull)
		category: #NautilusGlobalShortcuts
		default: $b meta , $f meta
		do: [ :target | target fullBrowse ]
		description: 'Open the same browser'.
	(aBuilder shortcut: #browseVersion)
		category: #NautilusGlobalShortcuts
		default: $m meta , $v meta
		do: [ :target | target browseVersions ]
		description: 'Browse senders of the selected method'.
	(aBuilder shortcut: #browseVariables)
		category: #NautilusGlobalShortcuts
		default: $b meta , $v meta
		do: [ :target | target browseVariableReferences ]
		description: 'Browse variables of the selected class'