keymapping
buildPatchShortcutsForSelectAllInWindowsOn: aBuilder
	<keymap>
	"this is because for some reason Ctrl+a means Ctrl+home on Windows"
	(aBuilder shortcut: #selectAllPatchForWin) 
		category: RubTextEditor name
		default: Character home ctrl win
		do: [ :target | target editor selectAll: nil ]
		description: 'Select all'
