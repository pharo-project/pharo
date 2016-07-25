private
loadLatestPackage: aString fromRepository: aRepository
	| versionsBlock versions tries |
	
	versionsBlock := [ (aRepository allVersionNames select: [ :each |
		each beginsWith: aString ])
		asSortedCollection: [ :a :b |
			(a copyAfterLast: $.) asNumber <= (b copyAfterLast: $.) asNumber]].
	versions := versionsBlock value.
	tries := 0.
	[ versions isEmpty and: [ tries < 3 ] ] whileTrue: [
		versions := versionsBlock value.
		tries := tries + 1 ].
	versions isEmpty ifTrue: [ self error: 'problems when accessing squeaksource' ].
	
	aRepository versionReaderForFileNamed: (versions last , '.mcz') do: [:reader | | version |
		version := reader version.
		version load.
		version workingCopy repositoryGroup addRepository: aRepository]