initialization
populateTreeFor: newModel
	| roots |
	roots := (KomitTreeBuilder rootsForStagingArea: newModel stagingArea) asOrderedCollection
		sorted.
	tree
		beCheckList;
		roots: roots;
		expandAll;
		collapseAll;
		expandRoots.
	self initializeLatelyPresenter.
	canCommit value: true