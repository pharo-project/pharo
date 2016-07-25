instance creation
fromFile: aFileReference

	^ self 
		named: aFileReference basenameWithoutExtension
		inFile: aFileReference