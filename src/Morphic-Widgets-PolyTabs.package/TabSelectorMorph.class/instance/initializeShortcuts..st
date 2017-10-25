keymapping
initializeShortcuts: aKMDispatcher
	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #MorphFocusNavigation.
	aKMDispatcher attachCategory: #TabMorphsNavigation.