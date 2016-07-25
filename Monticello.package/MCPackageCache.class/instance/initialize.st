initialization
initialize
	super initialize.
	sorter := MCVersionSorter new.
	fileNames := Dictionary new.