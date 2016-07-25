keyboard shortcut tables
initializeShiftCmdKeyShortcuts 
	"Initialize the shift-command-key (or control-key) shortcut table."
	"wod 11/3/1998: Fix setting of cmdMap for shifted keys to actually use the 
	capitalized versions of the letters.
	TPR 2/18/99: add the plain ascii values back in for those VMs that don't return the shifted values."

	"TextEditor initialize"
	
	| cmds |

	self initializeBasicShiftCmdKeyShortcuts.
	self shiftCmdActions at: (45 + 1) put: #changeEmphasis:.		"cmd-sh-minus"
	self shiftCmdActions at: (61 + 1) put: #changeEmphasis:.		"cmd-sh-plus"

	cmds := #(
		$k	changeStyle:
		"$u #clearUndoManager:	"
	).
	1 to: cmds size by: 2 do: [ :i |
		self shiftCmdActions at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).			"plain keys"
		self shiftCmdActions at: ((cmds at: i) asciiValue - 32 + 1) put: (cmds at: i + 1).		"shifted keys"
		self shiftCmdActions at: ((cmds at: i) asciiValue - 96 + 1) put: (cmds at: i + 1).		"ctrl keys"
	].