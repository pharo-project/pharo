archive operations
extractMember: aMemberOrName toFileNamed: aFileName
	| member |
	member := self member: aMemberOrName.
	member ifNil: [ ^nil ].
	member extractToFileNamed: aFileName