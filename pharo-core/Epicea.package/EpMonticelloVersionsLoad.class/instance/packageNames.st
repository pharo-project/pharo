accessing
packageNames

	^ versionNames collect: [ :each | each copyUpToLast: $- ]