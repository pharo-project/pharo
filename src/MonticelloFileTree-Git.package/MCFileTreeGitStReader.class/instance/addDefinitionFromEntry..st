utilities
addDefinitionFromEntry: aZipFileMember
	(self definitionFromEntry: aZipFileMember) ifNotNil: [ :def | definitions add: def ]