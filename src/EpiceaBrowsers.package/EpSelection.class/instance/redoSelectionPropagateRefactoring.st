operations
redoSelectionPropagateRefactoring

	| entries |
	entries := self entriesToRedo.

	self 
		trigger: [ entries do: [ :each |		
			self redoRefactoring: (EpRBPropagateRefactoring target: (each content asRBRefactoring)) ] ] 
		with: (EpRedo forAll: (browser log referencesToAll: entries))