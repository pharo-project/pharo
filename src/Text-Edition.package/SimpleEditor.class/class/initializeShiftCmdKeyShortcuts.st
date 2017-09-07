private - initialization
initializeShiftCmdKeyShortcuts 
	"Initialize the shift-command-key (or control-key) shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"
	"wod 11/3/1998: Fix setting of cmdMap for shifted keys to actually use the 
	capitalized versions of the letters.
	TPR 2/18/99: add the plain ascii values back in for those VMs that don't return the shifted values."

	| cmdMap |

	"shift-command and control shortcuts"
	cmdMap := Array new: 256 withAll: #noop:.  "use temp in case of a crash"
	"cmdMap at: ( 1 + 1) put: #cursorHome:."			"home key"
	"cmdMap at: ( 4 + 1) put: #cursorEnd:."			"end key"
	cmdMap at: ( 8 + 1) put: #forwardDelete:.		"ctrl-H or delete key"
	cmdMap at: (28 + 1) put: #cursorLeft:.			"left arrow key"
	cmdMap at: (29 + 1) put: #cursorRight:.			"right arrow key"
	cmdMap at: (27 + 1) put: #escape.					"escape"				
	cmdMap at: (32 + 1) put: #selectWord:.			"space bar key"
	cmdMap at: (127 + 1) put: #forwardDelete:.		"del key"

	#(
		$x	makeLowercase:
		$y	makeUppercase:
		$z	makeCapitalized:
	) pairsDo: [ :char :symbol|
		cmdMap at: char asciiValue + 1 put: symbol.
		cmdMap at: char asciiValue + 1 put: symbol.		"plain keys"
		cmdMap at: char asciiValue - 32 + 1 put:symbol.		"shifted keys"
		cmdMap at: char asciiValue - 96 + 1 put: symbol.		"ctrl keys"
	].
	ShiftCmdActions := cmdMap