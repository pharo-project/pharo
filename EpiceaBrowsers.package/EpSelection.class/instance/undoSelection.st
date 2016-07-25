operations
undoSelection

	| entries |
	entries := self entriesToUndo.

	self 
		trigger: [ entries do: [ :each |
			each content accept: EpUndoVisitor new ] ] 
		with: (EpUndo forAll: (browser log referencesToAll: entries))