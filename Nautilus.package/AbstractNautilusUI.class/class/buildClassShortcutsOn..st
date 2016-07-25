shortcut-old
buildClassShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #fullBrowse)
		category: #NautilusClassShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the selection'.
		
	(aBuilder shortcut: #restrictedBrowseClass)
		category: #NautilusClassShortcuts
		default: $b meta shift
		do: [ :target | target restrictedBrowseClass ]
		description: 'Open a restricted browser'.

	(aBuilder shortcut: #copyClasses)
		category: #NautilusClassShortcuts
		default: $c meta
		do: [ :target | target copyClasses ]
		description: 'Copy the selected classes'.
	
	(aBuilder shortcut: #findMethod)
		category: #NautilusClassShortcuts
		default: $f meta
		do: [ :target | target findMethod ]
		description: 'Find a method'.
		
	(aBuilder shortcut: #findClass)
		category: #NautilusClassShortcuts
		default: $f meta shift
		do: [ :target | target findClass ]
		description: 'Find a class'.
		
	(aBuilder shortcut: #generateInitialize)
		category: #NautilusClassShortcuts
		default: $i meta shift
		do: [ :target | target generateInitialize ]
		description: 'Generate the initialize method'.
		
	(aBuilder shortcut: #createTestForSelectedClass)
		category: #NautilusClassShortcuts
		default: $j meta
		do: [ :target | target enableSingleClassSelection ifTrue: [ target createTestForSelectedClass ]]
		description: 'Generate a test class for the selected class'.
		
	(aBuilder shortcut: #forceGenerateInitialize)
		category: #NautilusClassShortcuts
		default: $k meta shift
		do: [ :target | target forceGenerateInitialize ]
		description: 'Regenerate the initialize method'.
		
	(aBuilder shortcut: #browseClassRefs)
		category: #NautilusClassShortcuts
		default: $n meta shift
		do: [ :target | target browseClassRefs ]
		description: 'Browse class references'.
	
	(aBuilder shortcut: #addClass)
		category: #NautilusClassShortcuts
		default: $n meta
		do: [ :target | target addClass ]
		description: 'Add a class'.
		
	(aBuilder shortcut: #renameClass)
		category: #NautilusClassShortcuts
		default: $r meta
		do: [ :target | target enableSingleClassSelection ifTrue: [ target renameClass ]]
		description: 'Rename the selected class'.
		
	(aBuilder shortcut: #runClassTests)
		category: #NautilusClassShortcuts
		default: $t meta
		do: [ :target | target runClassTests ]
		description: 'Run the tests for the selected class'.
		
	(aBuilder shortcut: #removeClasses)
		category: #NautilusClassShortcuts
		default: $x meta
		do: [ :target | target removeClasses ]
		description: 'Remove the selected classes'