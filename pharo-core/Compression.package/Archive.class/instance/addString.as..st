archive operations
addString: aString as: aFileName
	| newMember |
	newMember := self memberClass newFromString: aString named: aFileName.
	self addMember: newMember.
	newMember localFileName: aFileName.
	^newMember