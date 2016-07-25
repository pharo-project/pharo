class initialization
initialize 
	"Initialize the keyboard shortcut maps and the shared buffers
	for copying text across views and managing again and undo." 
 
	"
	TextEditor initialize
	" 

	self initializeCmdKeyShortcuts.
	self initializeShiftCmdKeyShortcuts.
