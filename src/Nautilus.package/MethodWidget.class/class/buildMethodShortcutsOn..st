shortcuts
buildMethodShortcutsOn: aBuilder
	<keymap>	

	(aBuilder shortcut: #fullBrowse)
		category: #NautilusMethodShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the selection'.
		
	(aBuilder shortcut: #restrictedBrowseClass)
		category: #NautilusMethodShortcuts
		default: $b meta shift
		do: [ :target | target restrictedBrowseClass ]
		description: 'Open a restricted browser'.
		
	(aBuilder shortcut: #categorizeMethod)
		category: #NautilusMethodShortcuts
		default: $c meta shift
		do: [ :target | target categorizeMethod ]
		description: 'Recategorize the selected methods'.
	
	(aBuilder shortcut: #findMethod)
		category: #NautilusMethodShortcuts
		default: $f meta
		do: [ :target | target findMethod ]
		description: 'Find a method'.
	
	(aBuilder shortcut: #methodHierarchy)
		category: #NautilusMethodShortcuts
		default: $i meta
		do: [ :target | target enableMethodSingleSelection ifTrue: [ target methodHierarchy ]]
		description: 'Spawn selected method hierarchy'.
		
	(aBuilder shortcut: #generateTestMethodsAndFocus)
		category: #NautilusMethodShortcuts
		default: $j meta
		do: [ :target | target generateTestMethodsAndFocus: true ]
		description: 'Generate test methods for the selected methods and jump to them'.
		
	(aBuilder shortcut: #generateTestMethods)
		category: #NautilusMethodShortcuts
		default: $j meta shift
		do: [ :target | target generateTestMethodsAndFocus: false ]
		description: 'Generate test methods for the selected methods'.
		
	(aBuilder shortcut: #browseMessages)
		category: #NautilusMethodShortcuts
		default: $m meta
		do: [ :target | target enableMethodSingleSelection ifTrue: [ target browseMessages ]]
		description: 'Browse implementors'.
	
	(aBuilder shortcut: #browseSendersOfMessages)
		category: #NautilusMethodShortcuts
		default: $n meta
		do: [ :target | target enableMethodSingleSelection ifTrue: [ target browseSendersOfMessages ]]
		description: 'Browse senders'.
	
	(aBuilder shortcut: #runTestFor)
		category: #NautilusMethodShortcuts
		default: $t meta
		do: [ :target | target runTestForMethods: target selectedMethods notifying: true]
		description: 'Run the selected test methods'.
		
	(aBuilder shortcut: #browseVersions)
		category: #NautilusMethodShortcuts
		default: $v meta
		do: [ :target | target enableMethodSingleSelection ifTrue: [ target browseVersions ]]
		description: 'Browse version'.		
	
	(aBuilder shortcut: #removeMethods)
		category: #NautilusMethodShortcuts
		default: $x meta
		do: [ :target | target removeMethods ]
		description: 'Remove the selected methods'.
		
	(aBuilder shortcut: #toggleBytecodes)
		category: #NautilusMethodShortcuts
		default: $b meta
		do: [ :target | target toggleBytecodes ]
		description: 'ShowBytecodes'.