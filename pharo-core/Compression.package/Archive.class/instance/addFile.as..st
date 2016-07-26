archive operations
addFile: aFileName as: anotherFileName
	| newMember |
	newMember := self memberClass newFromFile: aFileName.
	newMember localFileName: anotherFileName.
	self addMember: newMember.
	^newMember