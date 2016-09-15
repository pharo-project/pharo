operations
applyChangesInSelection

	| entries |
	entries := self selectedCodeChanges reversed.
	entries ifEmpty: [ 
		^ self inform: 'No code changes to apply' ].

	self 
		trigger: [ entries do: [ :each | 
			each content accept: EpApplyVisitor new ] ] 
		with: (EpRedo forAll: (browser log referencesToAll: entries))