archive operations
addDirectory: aFileName as: anotherFileName
	| newMember |
	newMember := self memberClass newFromDirectory: aFileName.
	self addMember: newMember.
	newMember localFileName: anotherFileName.
	^newMember