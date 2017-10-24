keymapping
initializeShortcuts: aKMDispatcher

	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #SimpleHierarchicalList.
	aKMDispatcher attachCategory: #MorphFocusNavigation