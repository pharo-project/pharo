shortcuts
buildRunTestKeymappingsOn: aBuilder
	<keymap>
	(aBuilder shortcut: #runTestForClasses)
		category: #NautilusGlobalShortcuts
		default: $j meta , $c meta
		do: [ :target | target runClassTests ]
		description: 'runTestFor the selected classes'.
	(aBuilder shortcut: #runTestForPackages)
		category: #NautilusGlobalShortcuts
		default: $j meta , $p meta
		do: [ :target | target runPackagesTestsNotifying: true ]
		description: 'runTestFor the selected packages'.
	(aBuilder shortcut: #runTestForMethods)
		category: #NautilusGlobalShortcuts
		default: $j meta , $m meta
		do: [ :target | target runTestForMethods: target selectedMethods notifying: true ]
		description: 'runTestFor the selected methods'