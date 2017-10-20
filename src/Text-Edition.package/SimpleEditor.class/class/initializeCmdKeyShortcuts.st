private - initialization
initializeCmdKeyShortcuts
	"Initialize the (unshifted) command-key (or alt-key) shortcut table."

	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"

	"
	SimpleEditor initialize
	"

	| cmdMap cmds |
	cmdMap := Array new: 256 withAll: #noop:.	"use temp in case of a crash"
	"cmdMap at: 1 + 1 put: #cursorHome:."	"home key"
	"cmdMap at: 4 + 1 put: #cursorEnd:."	"end key"
	cmdMap at: 8 + 1 put: #backspace:.	"ctrl-H or delete key"
	cmdMap at: 27 + 1 put: #escape:.	"left arrow key"
	cmdMap at: 28 + 1 put: #cursorLeft:.	"left arrow key"
	cmdMap at: 29 + 1 put: #cursorRight:.	"right arrow key"
	cmdMap at: 32 + 1 put: #selectWord:.	"space bar key"
	cmdMap at: 127 + 1 put: #forwardDelete:.	"del key"
	
	cmds := #("$a #selectAll: $c #copySelection: $v #paste:" $w #backWord: "$x #cut:" ).
	1 to: cmds size
		by: 2
		do: [:i | cmdMap at: (cmds at: i) asciiValue + 1 put: (cmds at: i + 1)].
	CmdActions := cmdMap