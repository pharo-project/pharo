keymapping
initializeShortcuts: aKMDispatcher 
	"Where we may attach keymaps or even on:do: local shortcuts if needed."

	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #WindowShortcuts	