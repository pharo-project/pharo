keyboard shortcut tables
initializeCmdKeyShortcuts
	"Initialize the (unshifted) command-key (or alt-key) shortcut table."

	"NOTE: if you don't know what your keyboard generates, use Sensor kbdTest"

	"SmalltalkEditor initialize"

	| cmds |
	self initializeBasicCmdKeyShortcuts.
	
	cmds := #(
		"$b #browseIt: "
		$d #doIt: 
		"$i #inspectIt:" 
		"$z #undo:" 
		$j #redo: 
		$l #cancel: 
		$m #implementorsOfIt: 
		"$n #sendersOfIt:" 
		"$p #printIt: "
		$q #querySymbol: 
		"$s #save:" ).
	1 to: cmds size
		by: 2
		do: [:i | cmdActions at: (cmds at: i) asciiValue + 1 put: (cmds at: i + 1)].
 