accessing
fileEntryRemove: aFileName ifAbsent: absentBlock
	| deletedEntry |
	deletedEntry := entries removeKey: aFileName ifAbsent: [ ^ absentBlock value ].
	modificationTime := DateAndTime now.
	^ deletedEntry