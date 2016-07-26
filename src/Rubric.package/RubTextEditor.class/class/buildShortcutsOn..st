keymapping
buildShortcutsOn: aBuilder
	"We are defining the bindings twice because we want to support 
	both Cmd and Ctrl for Windows and Linux. This should happen at least as long as in the development environment
	both of these are supported.
	
	We list both variations explicitly because we want to be able to see the action code when inspecting the morph.
	"
	<keymap>
	(aBuilder shortcut: #cancel) 
		category: RubTextEditor name
		default: $l meta
		do: [ :target | target editor cancel ]
		description: 'Cancel unsaved editings'.
		
	(aBuilder shortcut: #accept) 
		category: RubTextEditor name
		default: $s meta
		do: [ :target | target editor accept ]
		description: 'Accept unsaved editings'.
	
	(aBuilder shortcut: #selectAll) 
		category: RubTextEditor name
		default: $a meta
		do: [ :target | target editor selectAll: nil ]
		description: 'Select all'.

	(aBuilder shortcut: #copySelection) 
		category: RubTextEditor name
		default: $c meta
		do: [ :target | target editor copySelection ]
		description: 'Copy selection'.

	(aBuilder shortcut: #paste) 
		category: RubTextEditor name
		default: $v meta
		do: [ :target | target editor paste ]
		description: 'Paste'.

	(aBuilder shortcut: #cut) 
		category: RubTextEditor name
		default: $x meta
		do: [ :target | target editor cut ]
		description: 'Cut selection'.

	(aBuilder shortcut: #undometa) 
		category: RubTextEditor name
		default: $z meta
		do: [ :target | target editor undo ]
		description: 'Undo'.

	(aBuilder shortcut: #redometa) 
		category: RubTextEditor name
		default: $z meta shift
		do: [ :target | target editor redo ]
		description: 'Redo'.

	(aBuilder shortcut: #find) 
		category: RubTextEditor name
		default: $f meta
		do: [ :target | target editor find: nil ]
		description: 'Find text'.

	(aBuilder shortcut: #findAgainmeta) 
		category: RubTextEditor name
		default: $g meta
		do: [ :target | target editor findAgain: nil ]
		description: 'Find text again'.
		
	(aBuilder shortcut: #indent) 
		category: RubTextEditor name
		default: $r meta shift
		do: [ :target | target editor indent: nil ]
		description: 'Indent'.

	(aBuilder shortcut: #outdent) 
		category: RubTextEditor name
		default: $l meta shift
		do: [ :target | target editor outdent: nil ]
		description: 'Outdent'.

	(aBuilder shortcut: #compareToClipboard) 
		category: RubTextEditor name
		default: $c meta shift
		do: [ :target | target editor compareToClipboard: nil ]
		description: 'Compare selection to clipboard'.