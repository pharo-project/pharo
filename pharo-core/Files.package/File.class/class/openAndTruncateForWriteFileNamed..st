file creation
openAndTruncateForWriteFileNamed: aName

	^ (self named: aName)
		delete;
		openForWrite