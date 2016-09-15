operations
filterAllChangesToSelectionPackage

	| changes |
	changes := (self selectedEntryItems collect: [ :each | each entry content ]).

	changes size = 1 ifFalse: [ self notYetImplemented ]. 

	self filterWith: (EpPluggableFilter isPackage: changes anyOne affectedPackageName).