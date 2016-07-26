operations
filterAllChangesToSelectionPackage

	| changes |
	changes := (self itemsForMenuAction collect: [ :each | each entry content ]).

	changes size = 1 ifFalse: [ self notYetImplemented ]. 

	self filterWith: (EpPluggableFilter isPackage: changes anyOne affectedPackageName).