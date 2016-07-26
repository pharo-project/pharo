operations
redoSelection

	| entries |
	entries := self entriesToRedo.

	self 
		trigger: [ entries do: [ :each | 
			each content accept: EpRedoVisitor new ] ] 
		with: (EpRedo forAll: (browser log referencesToAll: entries))