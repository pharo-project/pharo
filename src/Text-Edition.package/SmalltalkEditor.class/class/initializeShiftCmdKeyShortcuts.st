keyboard shortcut tables
initializeShiftCmdKeyShortcuts 
	"Initialize the shift-command-key (or control-key) shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"
	"wod 11/3/1998: Fix setting of cmdMap for shifted keys to actually use the 
	capitalized versions of the letters.
	TPR 2/18/99: add the plain ascii values back in for those VMs that don't return the shifted values."

	"SmalltalkEditor initialize"

	| cmds |
 	self initializeBasicShiftCmdKeyShortcuts.
	
	cmds := #(
		$a	argAdvance:
		$d	debugIt:
		$e	methodStringsContainingIt:
		$f	displayIfFalse:
		$g	fileItIn:
		$i	exploreIt:
		$n	referencesToIt:
		$t	displayIfTrue:
		$v	pasteInitials:
		$w	methodNamesContainingIt:
	).
	1 to: cmds size by: 2 do: [ :i |
		shiftCmdActions at: ((cmds at: i) asciiValue + 1) put: (cmds at: i + 1).			"plain keys"
		shiftCmdActions at: ((cmds at: i) asciiValue - 32 + 1) put: (cmds at: i + 1).		"shifted keys"
		shiftCmdActions at: ((cmds at: i) asciiValue - 96 + 1) put: (cmds at: i + 1).		"ctrl keys"
	].