shortcut-old
buildPackageShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #fullBrowse)
		category: #NautilusPackageShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the selection'.
		
	(aBuilder shortcut: #findClass)
		category: #NautilusPackageShortcuts
		default: $f meta
		do: [ :target | target findClass ]
		description: 'Find a class'.
		
	(aBuilder shortcut: #findPackage)
		category: #NautilusPackageShortcuts
		default: $f meta shift
		do: [ :target | target findPackage ]
		description: 'Find a package'.
		
	(aBuilder shortcut: #addPackage)
		category: #NautilusPackageShortcuts
		default: $n meta 
		do: [ :target | target addPackage ]
		description: 'Add a new package'.
	
	(aBuilder shortcut: #renamePackage)
		category: #NautilusPackageShortcuts
		default: $r meta 
		do: [ :target | target enableSingleMenuItems ifTrue: [ target renamePackage ]]
		description: 'Rename the selected package'.	
	
	(aBuilder shortcut: #runPackagesTests)
		category: #NautilusPackageShortcuts
		default: $t meta 
		do: [ :target | target runPackagesTestsNotifying: true ]
		description: 'Run the tests for the selected packages'.
	
	(aBuilder shortcut: #removePackages)
		category: #NautilusPackageShortcuts
		default: $x meta 
		do: [ :target | target removePackages ]
		description: 'Remove the selected packages'.