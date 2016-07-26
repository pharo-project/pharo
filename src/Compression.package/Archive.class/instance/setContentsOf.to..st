archive operations
setContentsOf: aMemberOrName to: aString
	| newMember oldMember |
	oldMember := self member: aMemberOrName.
	newMember := (self memberClass newFromString: aString named: oldMember fileName)
		copyFrom: oldMember.
	self replaceMember: oldMember with: newMember.