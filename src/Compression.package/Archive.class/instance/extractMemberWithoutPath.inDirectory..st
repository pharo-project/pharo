archive operations
extractMemberWithoutPath: aMemberOrName inDirectory: dir
	| member |
	member := self member: aMemberOrName.
	member ifNil: [ ^nil ].
	member extractToFileNamed: (member asFileReference basename) inDirectory: dir