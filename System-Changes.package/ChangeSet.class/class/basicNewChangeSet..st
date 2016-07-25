enumerating
basicNewChangeSet: newName
	| newSet |
	newName ifNil: [^ nil].
	(self named: newName) ifNotNil:
		[self inform: 'Sorry that name is already used'.
		^ nil].
	newSet := self basicNewNamed: newName.
	AllChangeSets add: newSet.
	^ newSet