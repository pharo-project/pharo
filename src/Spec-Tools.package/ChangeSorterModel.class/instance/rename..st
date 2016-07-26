change set
rename: aChangeSet
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	| newName |
	newName := UIManager default request: 'New name for this change set'
						initialAnswer: aChangeSet name.
						
	(newName = aChangeSet name or: [newName isEmptyOrNil]) ifTrue:
			[ ^ UIManager default inform: 'Please give a new name' ].

	(ChangeSet named: newName) ifNotNil:
			[ ^ UIManager default inform: 'Sorry that name is already used' ].

	aChangeSet name: newName.
	^ aChangeSet