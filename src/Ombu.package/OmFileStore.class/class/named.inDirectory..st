instance creation
named: aName inDirectory: baseDirectoryFileReference

	^ self named: aName inFile: (baseDirectoryFileReference / (aName, self defaultFileSuffix))