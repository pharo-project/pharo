protocol
selectedGroup
	^ self selectedGroups ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]