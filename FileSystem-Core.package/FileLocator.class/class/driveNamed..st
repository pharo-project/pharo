windows-origins
driveNamed: driveName
	^ FileReference fileSystem: (FileSystem disk) path: Path / (driveName, ':')