current changeset
newChanges: aChangeSet withOld: old
	"Set the system ChangeSet to be the argument, aChangeSet."

	SystemAnnouncer uniqueInstance unsubscribe: old.
	current := aChangeSet.
	
	SystemAnnouncer uniqueInstance private weak
		when: ClassRemoved send: #classRemoved: to: aChangeSet;
		when: ClassAdded send: #classAdded: to: aChangeSet;
		when: ClassCommented send: #classCommented: to: aChangeSet;
		when: ClassRenamed send: #classRenamed: to: aChangeSet;
		when: ClassReorganized send: #classReorganized: to: aChangeSet;
		when: ClassRecategorized send: #classRecategorized: to: aChangeSet;
		when: ClassModifiedClassDefinition send: #classModified: to: aChangeSet.
	
	SystemAnnouncer uniqueInstance private weak
		when: MethodAdded send: #methodAdded: to: aChangeSet;
		when: MethodModified send: #methodModified: to: aChangeSet;
		when: MethodRemoved send: #methodRemoved: to: aChangeSet;
		when: MethodRecategorized  send: #methodRecategorized: to: aChangeSet.
		
	
	SystemAnnouncer uniqueInstance announce: (CurrentChangeSetChanged new old: old; new: aChangeSet ; yourself).
	