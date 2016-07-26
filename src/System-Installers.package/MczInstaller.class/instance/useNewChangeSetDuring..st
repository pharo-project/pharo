utilities
useNewChangeSetDuring: aBlock
	| changeHolder oldChanges newChanges |
	changeHolder := (ChangeSet respondsTo: #newChanges:)
						ifTrue: [ChangeSet]
						ifFalse: [Smalltalk].
	oldChanges := (ChangeSet respondsTo: #current)
						ifTrue: [ChangeSet current]
						ifFalse: [Smalltalk changes].

	newChanges := ChangeSet new name: (ChangeSet uniqueNameLike: self extractPackageName).
	changeHolder newChanges: newChanges.
	[aBlock value] ensure: [changeHolder newChanges: oldChanges].