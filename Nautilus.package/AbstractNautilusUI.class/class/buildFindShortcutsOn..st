shortcuts
buildFindShortcutsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #findClass)
		category: #NautilusGlobalShortcuts
		default: $f meta , $c meta
		do: [ :target | target findClass ]
		description: 'Find a class'.
	(aBuilder shortcut: #findClassInHierarchy)
		category: #NautilusGlobalShortcuts
		default: $f meta , $h meta
		do: [ :target | target findClassInHierarchy ]
		description: 'Find a class in the hierarchy'.
	(aBuilder shortcut: #findSuperclass)
		category: #NautilusGlobalShortcuts
		default: $f meta , $S meta shift
		do: [ :target | target findSuperclass ]
		description: 'Find a superclass'.
	(aBuilder shortcut: #findSubclass)
		category: #NautilusGlobalShortcuts
		default: $f meta , $s meta
		do: [ :target | target findSubclass ]
		description: 'Find a subclass'.
	(aBuilder shortcut: #findPackage)
		category: #NautilusGlobalShortcuts
		default: $f meta , $p meta
		do: [ :target | target findPackage ]
		description: 'Find a package'.
	(aBuilder shortcut: #findProtocol)
		category: #NautilusGlobalShortcuts
		default: $f meta , $t meta
		do: [ :target | target findProtocol ]
		description: 'Find a protocol'.
	(aBuilder shortcut: #findMethod)
		category: #NautilusGlobalShortcuts
		default: $f meta , $m meta
		do: [ :target | target findMethod ]
		description: 'Find a method'