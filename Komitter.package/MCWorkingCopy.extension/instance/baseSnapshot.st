*Komitter
baseSnapshot

	^ self ancestors
		ifEmpty: [ nil ]
		ifNotEmpty: [ :ancestors | self findSnapshotWithVersionInfo: ancestors first ]