keymapping
buildShortcutsOn: aBuilder
	"We are defining the bindings twice because we want to support 
	both Cmd and meta for Windows and Linux. This should happen at least as long as in the development environment
	both of these are supported.
	
	We list both variations explicitly because we want to be able to see the action code when inspecting the morph.
	"

	<keymap>
	(aBuilder shortcut: #querySymbol)
		category: RubSmalltalkEditor name
		default: $q meta
		do: [ :target | target editor querySymbol: nil ]
		description: 'Query symbol'.
	
	(aBuilder shortcut: #browseIt)
		category: RubSmalltalkEditor name
		default: $b meta
		do: [ :target | target editor browseIt: nil ]
		description: 'Browse'.
	
	(aBuilder shortcut: #doIt)
		category: RubSmalltalkEditor name
		default: $d meta
		do: [ :target | target editor doIt: nil ]
		description: 'Do it'.
	
	(aBuilder shortcut: #inspectIt)
		category: RubSmalltalkEditor name
		default: $i meta
		do: [ :target | target editor inspectIt: nil ]
		description: 'Inspect it'.
	
	(aBuilder shortcut: #basicInspectIt)
		category: RubSmalltalkEditor name
		default: $i meta shift
		do: [ :target | target editor basicInspectIt ]
		description: 'Basic Inspect it'.
	
	(aBuilder shortcut: #implementorsOfIt)
		category: RubSmalltalkEditor name
		default: $m meta
		do: [ :target | target editor implementorsOfIt: nil ]
		description: 'Implementors of it'.
	
	(aBuilder shortcut: #senderOfIt)
		category: RubSmalltalkEditor name
		default: $n meta
		do: [ :target | target editor sendersOfIt: nil ]
		description: 'Senders of it'.
	
	(aBuilder shortcut: #printIt)
		category: RubSmalltalkEditor name
		default: $p meta
		do: [ :target | target editor printIt ]
		description: 'Print it'.
	
	(aBuilder shortcut: #debugIt)
		category: RubSmalltalkEditor name
		default: $d meta shift
		do: [ :target | target editor debugIt: nil ]
		description: 'Debug it'.
	
	(aBuilder shortcut: #referencesToIt)
		category: RubSmalltalkEditor name
		default: $n meta shift
		do: [ :target | target editor referencesToIt: nil ]
		description: 'References to it'.
	
	(aBuilder shortcut: #methodStringsContainingIt)
		category: RubSmalltalkEditor name
		default: $e meta shift
		do: [ :target | target editor methodStringsContainingIt: nil ]
		description: 'Method strings containing it'.
	
	(aBuilder shortcut: #format)
		category: RubSmalltalkEditor name
		default: PharoShortcuts current formatCodeShortcut
		do: [ :target | target formatMethodCode ]