operations
revertChangesInSelection

	| entries |
	entries := self selectedCodeChanges.
	entries ifEmpty: [ 
		^ self inform: 'No code changes to revert' ].

	self 
		trigger: [ entries do: [:each | each content accept: EpRevertVisitor new ]]
		with: (EpUndo forAll: (browser log referencesToAll: entries))