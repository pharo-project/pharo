menu - filters
filterAllChangesToSelectionPackage

	| changes |
	changes := (self selectedEntryItems collect: [ :each | each entry content ]).

	changes size = 1 ifFalse: [ ^ self inform: 'Should select a single change' ]. 

	self addFilter: (EpPluggableFilter isPackageNamed: changes anyOne affectedPackageName).