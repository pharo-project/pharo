archive operations
membersMatching: aString
	^members select: [ :ea | (aString match: ea fileName) or: [ aString match: ea localFileName ] ]