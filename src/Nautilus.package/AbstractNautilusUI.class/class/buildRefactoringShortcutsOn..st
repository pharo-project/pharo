shortcuts
buildRefactoringShortcutsOn: aBuilder
	<keymap>

	(aBuilder shortcut: #generateInitialize)
		category: #NautilusGlobalShortcuts
		default: $h meta, $i meta 
		do: [ :target | target generateInitialize ]
		description: 'Generate the initialize method'.
		
	(aBuilder shortcut: #forceGenerateInitialize)
		category: #NautilusGlobalShortcuts
		default: $h meta, $k meta
		do: [ :target | target forceGenerateInitialize ]
		description: 'Regenerate the initialize method'.
		
	(aBuilder shortcut: #createTestForSelectedClass)
		category: #NautilusGlobalShortcuts
		default: $n meta, $j meta, $c meta
		do: [ :target | target enableSingleClassSelection ifTrue: [ target createTestForSelectedClass ]]
		description: 'Generate a test class for the selected class'.
		
	(aBuilder shortcut: #generateTestAndJump)
		category: #NautilusGlobalShortcuts
		default: $h meta, $j meta
		do: [ :target | target generateTestMethodsAndFocus: true ]
		description: 'Generate test and jump'.
		
	(aBuilder shortcut: #generateTest)
		category: #NautilusGlobalShortcuts
		default: $h meta, $j meta shift
		do: [ :target | target generateTestMethodsAndFocus: false ]
		description: 'Generate test'.
		
	(aBuilder shortcut: #categorizeAllUncategorized)
		category: #NautilusGlobalShortcuts
		default: $h meta, $c meta shift
		do: [ :target | target categorizeAllUncategorizedMethods ]
		description: 'Categorize all uncategorized and remove empty'.