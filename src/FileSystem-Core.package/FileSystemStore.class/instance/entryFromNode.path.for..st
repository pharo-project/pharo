public
entryFromNode: node path: path for: aFileystem
	| entryPath |
	entryPath := path / (self basenameFromEntry: node).
	^ self entryFromNode: node fileSystem: aFileystem path: entryPath