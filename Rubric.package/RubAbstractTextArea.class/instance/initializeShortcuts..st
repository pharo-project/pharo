keymapping
initializeShortcuts: aKMDispatcher
	"we delegate the creation of default shortcuts to the editor
	because it is the one that knows more details about the goal of editing"
	super initializeShortcuts: aKMDispatcher.
	self editor initializeShortcuts: aKMDispatcher