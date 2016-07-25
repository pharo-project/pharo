operations
renameTo: aNewName
	(aNewName isNil or: [aNewName isEmpty]) ifTrue: [^ self].
	self name: aNewName