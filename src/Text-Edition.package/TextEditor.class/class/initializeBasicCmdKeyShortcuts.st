keyboard shortcut tables
initializeBasicCmdKeyShortcuts
	"Initialize the (unshifted) command-key (or alt-key) shortcut table.
	
	TextEditor initialize
	"
	| cmdMap |
	
	cmdMap := Array new: 256 withAll: #noop:.		"use temp in case of a crash"
	cmdMap at: 1 + 1 put: #cursorHome:.				"home key"
	cmdMap at: 4 + 1 put: #cursorEnd:.				"end key"
	cmdMap at: 8 + 1 put: #backspace:.				"ctrl-H or delete key"
	cmdMap at: 11 + 1 put: #cursorPageUp:.			"page up key"
	cmdMap at: 12 + 1 put: #cursorPageDown:.		"page down key"
	cmdMap at: 13 + 1 put: #crWithIndent:.			"cmd-Return"
	cmdMap at: 27 + 1 put: #escape:.		"escape key"
	cmdMap at: 28 + 1 put: #cursorLeft:.				"left arrow key"
	cmdMap at: 29 + 1 put: #cursorRight:.			"right arrow key"
	cmdMap at: 30 + 1 put: #cursorUp:.				"up arrow key"
	cmdMap at: 31 + 1 put: #cursorDown:.			"down arrow key"
	"cmdMap at: 32 + 1 put: #selectWord:."			"space bar key"
	cmdMap at: 127 + 1 put: #forwardDelete:.		"del key"
	"cmdMap at: 115 + 1 put:#accept:".	
		
	#(
		$a #selectAll: 
		"$c #copySelection:" 
		$e #exchange: 
		"$f #find: "
		"$g #findAgain: "
		"$h #setSearchString: "
		"$v #paste: "
		$w #backWord: 
		"$x #cut:" 
		$y #swapChars: 
		"$z #undo:" 
		$j #redo: 
		"$u #exploreUndoManager: ")
	pairsDo: [ :char :symbol|
		cmdMap at: char asciiValue + 1 put: symbol].
		
	cmdActions := cmdMap