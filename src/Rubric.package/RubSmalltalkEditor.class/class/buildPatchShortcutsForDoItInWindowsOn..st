keymapping
buildPatchShortcutsForDoItInWindowsOn: aBuilder
	"this is because for some reason Ctrl+d means Ctrl+end on Windows"
	<keymap>
	(aBuilder shortcut: #doItPatchForWindows) 
		category: RubSmalltalkEditor name
		default: Character end ctrl win
		do: [ :target | target editor doIt: nil ]
		description: 'Do it'.
	
	(aBuilder shortcut: #debugItPatchForWindows) 
		category: RubSmalltalkEditor name
		default: Character end shift ctrl win
		do: [ :target | target editor debugIt: nil ]
		description: 'Debug it'